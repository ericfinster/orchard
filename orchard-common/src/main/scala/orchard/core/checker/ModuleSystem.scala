/**
  * ModuleSystem.scala - Common traits and classes for server and client module systems
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.checker

import orchard.core.util.ErrorM._

trait ModuleSystem {

  type NodeType <: Node
  type ModuleNodeType <: NodeType with ModuleNode
  type ParameterNodeType <: NodeType with ParameterNode
  type DefinitionNodeType <: NodeType with DefinitionNode
  type ImportNodeType <: NodeType with ImportNode


  trait Node {
    thisNode : NodeType =>

    def name : String 

    var address : Vector[Int] = Vector.empty
  }

  trait ModuleNode extends Node { 
    thisNode : ModuleNodeType =>

    def insertEntryAt(me : ModuleEntry, i : Int) 
  }

  trait ParameterNode extends Node { 
    thisNode : ParameterNodeType =>
  }

  trait DefinitionNode extends Node {
    thisNode : DefinitionNodeType =>
  }

  trait ImportNode extends Node {
    thisNode : ImportNodeType =>
  }

  //============================================================================================
  // MODULE ENTRIES
  //

  sealed trait ModuleEntry { def node : NodeType }

  case class Module(val moduleNode : ModuleNodeType, val entries : Vector[ModuleEntry]) extends ModuleEntry {
    def node = moduleNode
  }

  case class Parameter(val parameterNode : ParameterNodeType) extends ModuleEntry {
    def node = parameterNode
  }

  case class Definition(val definitionNode : DefinitionNodeType) extends ModuleEntry { 
    def node = definitionNode
  }

  case class Import(val importNode : ImportNodeType) extends ModuleEntry {
    def node = importNode
  }

  //============================================================================================
  // MODULE CONTEXTS AND ZIPPERS
  //

  case class ModuleContext(node : ModuleNodeType, left : Vector[ModuleEntry], right : Vector[ModuleEntry]) {

    def insertAfter(entry : ModuleEntry) : ModuleContext =
      ModuleContext(node, left, entry +: right)

    def insertBefore(entry : ModuleEntry) : ModuleContext =
      ModuleContext(node, left :+ entry, right)

  }


  case class ModuleZipper(val focus : ModuleEntry, val context : List[ModuleContext]) {

    def zipOnce : Error[ModuleZipper] =
      context match {
        case Nil => fail("Zip failed: emtpy context.")
        case ModuleContext(node, left, right) :: cs =>
          success(ModuleZipper(Module(node, left ++ Vector(focus) ++ right), cs))
      }

    def zip : ModuleEntry =
      context match {
        case Nil => focus
        case ModuleContext(node, left, right) :: cs =>
          ModuleZipper(Module(node, left ++ Vector(focus) ++ right), cs).zip
      }

    def collectLefts : Vector[ModuleEntry] = 
      context match {
        case Nil => Vector.empty
        case ModuleContext(node, left, right) :: cs => {
          ModuleZipper(Module(node, left ++ Vector(focus) ++ right), cs).collectLefts ++ left
        }
      }

    def toAddress : Vector[Int] =
      context match {
        case Nil => Vector.empty
        case ModuleContext(node, left, right) :: cs =>
          ModuleZipper(Module(node, left ++ Vector(focus) ++ right), cs).toAddress :+ left.length
      }

    def insertAfter(entry : ModuleEntry) : Error[ModuleZipper] =
      context match {
        case Nil => fail("Cannot insert: no module open")
        case c :: cs => success(ModuleZipper(focus, c.insertAfter(entry) :: cs))
      }

    def insertBefore(entry : ModuleEntry) : Error[ModuleZipper] =
      context match {
        case Nil => fail("Cannot insert: no module open")
        case c :: cs => success(ModuleZipper(focus, c.insertBefore(entry) :: cs))
      }

    // Warning! The semantics of this method are a bit different from the two preceding it.
    def insertAt(entry : ModuleEntry, i : Int) : Error[ModuleZipper] =
      focus match {
        case m : Module => {
          val (left, right) = m.entries.splitAt(i)

          // Now, we should have the insertion routine pass to all of the
          // *right* entries and update their addresses.

          // Then we can pass the module itself directly to an insertion routine
          // on the module description so that all of the stateful updates are
          // hidden by this purely functional routine.

          m.node.insertEntryAt(entry, i)
          
          val zip = ModuleZipper(m.copy(entries = left ++ Vector(entry) ++ right), context)

          // Now, for the values in the correct range, update all the addresses...

          for {
            k <- Range(left.length, m.entries.length + 1)
            child <- zip.visitEntry(k)
          } {
            child.foreachWithAddress {
              case (addr, node) => {
                println("Setting address of " ++ node.name ++ " to " ++ addr.toString)
                node.address = addr
              }
            }
          }

          success(zip)
        }
        case _ => fail("Focused entry cannot have children")
      }
    
    def appendEntry(entry : ModuleEntry) : Error[ModuleZipper] =
      focus match {
        case m : Module =>
          success(ModuleZipper(m.copy(entries = m.entries :+ entry), context))
        case _ => fail("Focused entry cannot have children.")
      }

    def preprendEntry(entry : ModuleEntry) : Error[ModuleZipper] =
      focus match {
        case m : Module =>
          success(ModuleZipper(m.copy(entries = entry +: m.entries), context))
        case _ => fail("Focused entry cannot have children.")
      }

    def visitEntry(i : Int) : Error[ModuleZipper] =
      focus match {
        case m : Module =>
          if (i < 0 || i > m.entries.length - 1)
            fail("No entry at index " ++ i.toString)
          else {
            val (left, rightPlus) = m.entries.splitAt(i)
            success(ModuleZipper(rightPlus.head, ModuleContext(m.node, left, rightPlus.tail) :: context))
          }
        case _ => fail("Focused module has no children.")
      }

    def seek(addr : Vector[Int]) : Error[ModuleZipper] =
      if (addr.length <= 0) {
        success(this)
      } else {
        for {
          entry <- visitEntry(addr.head)
          res <- entry.seek(addr.tail)
        } yield res
      }

    //   def firstChild : Error[ModuleZipper] = {
    //     focus match {
    //       case m : Module =>
    //         if (m.entries.length > 0) {
    //           ???
    //         } else fail("Focused entry does not have children.")
    //       case _ => fail("Focused entry does not have children.")
    //     }
    //   }

    def foreachWithAddress(f : Function2[Vector[Int], NodeType, Unit]) : Unit =
      focus match {
        case m : Module => {
          for {
            i <- Range(0, m.entries.length)
          } {
            println("Visiting index " ++ i.toString ++ " on module " ++ m.node.name)
            visitEntry(i).get.foreachWithAddress(f)
          }

          f(toAddress, m.node)
        }
        case p : Parameter => f(toAddress, p.node)
        case d : Definition => f(toAddress, d.node)
      }

    def focusedModule : Error[Module] =
      focus match {
        case m : Module => success(m)
        case _ => fail("Focused entry is not a module.")
      }
  }
  

}
