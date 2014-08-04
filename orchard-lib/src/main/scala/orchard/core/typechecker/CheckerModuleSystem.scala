/**
  * CheckerModuleSystem.scala - Modules for the type checker
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.typechecker 

import orchard.core.util.ErrorM._

trait CheckerModuleSystem { thisChecker : ModuleChecker =>

  //============================================================================================
  // MODULE ENTRIES
  //

  sealed trait ModuleEntry

  sealed trait ContainerEntry extends ModuleEntry { def entries : Vector[ModuleEntry] }
  sealed trait ExpressionEntry extends ModuleEntry { def expression : Expression }

  case class Module(val moduleNode : ModuleNode, val entries : Vector[ModuleEntry]) extends ContainerEntry
  case class Import(val importNode : ImportNode, val entries : Vector[ModuleEntry]) extends ContainerEntry
  case class Parameter(val parameterNode : ParameterNode) extends ModuleEntry
  case class Definition(val definitionNode : DefinitionNode) extends ModuleEntry

  //============================================================================================
  // NODES
  //

  sealed trait Node

  sealed trait ContainerNode extends Node
  sealed trait ExpressionNode extends Node

  trait ModuleNode extends ContainerNode
  trait ImportNode extends ContainerNode
  trait ParameterNode extends ExpressionNode
  trait DefinitionNode extends ExpressionNode

  // Okay, because this is all functional, I don't think we should store the actual nodes in
  // the tree themselves, because these can under any update operations.  I think you have to
  // do it by address.  It seems like the only way ...

  class ModuleDefinition(val moduleName : String, val worksheets : Vector[Worksheet]) extends ModuleNode
  class ImportDefinition(val importName : String, val module : Module) extends ImportNode
  class ParameterDefinition(val variable : Variable) extends ParameterNode
  class DefinitionDefinition(val filler : Filler) extends DefinitionNode

  sealed trait ImportedNode
  class ImportedModule(moduleNode : ModuleNode) extends ImportedNode with ModuleNode
  class ImportedImport(importNode : ImportNode) extends ImportedNode with ImportNode
  class ImportedParameter(parameterNode : ParameterNode) extends ImportedNode with ParameterNode
  class ImportedDefinition(definitionNode : DefinitionNode) extends ImportedNode with DefinitionNode

  //============================================================================================
  // MODULE CONTEXTS AND ZIPPERS
  //

  case class ModuleContext(val node : ContainerNode, val left : Vector[ModuleEntry], val right : Vector[ModuleEntry])

  case class ModuleZipper(val focus : ModuleEntry, val context : List[ModuleContext]) {

    def seal(node : ContainerNode, entries : Vector[ModuleEntry]) : ModuleEntry =
      node match {
        case moduleNode : ModuleNode => Module(moduleNode, entries)
        case importNode : ImportNode => Import(importNode, entries)
      }

    def parent : Error[ModuleZipper] =
      context match {
        case Nil => fail("Zip failed: emtpy context.")
        case ModuleContext(node, left, right) :: cs =>
          success(ModuleZipper(seal(node, left ++ Vector(focus) ++ right), cs))
      }

    def zip : ModuleEntry =
      parent match {
        case Left(_) => focus
        case Right(p) => p.zip
      }

    def upperSlice : Vector[ModuleEntry] = 
      context match {
        case Nil => Vector.empty
        case ModuleContext(node, left, right) :: cs => {
          ModuleZipper(seal(node, left ++ Vector(focus) ++ right), cs).upperSlice ++ left
        }
      }

    def upperSliceWithAddress : Vector[(ModuleEntry, Vector[Int])] = 
      parent match {
        case Left(_) => Vector.empty
        case Right(parent) => {
          parent.upperSliceWithAddress ++ leftSiblingsWithAddresses
        }
      }

    def leftSiblingsWithAddresses : Vector[(ModuleEntry, Vector[Int])] = 
      leftSibling match {
        case Right(zipper) => 
          zipper.leftSiblingsWithAddresses :+ (focus -> toAddress)
        case Left(_) => Vector.empty
      }

    def leftSibling : Error[ModuleZipper] = 
      context match {
        case Nil => fail("Cannot move left in empty context")
        case ModuleContext(node, left, right) :: cs => {
          if (left.length < 1) {
            fail("No left sibling")
          } else {
            success(ModuleZipper(left.last, ModuleContext(node, left.init, focus +: right) :: cs))
          }
        }
      }

    def toAddress : Vector[Int] =
      context match {
        case Nil => Vector.empty
        case ModuleContext(node, left, right) :: cs =>
          ModuleZipper(seal(node, left ++ Vector(focus) ++ right), cs).toAddress :+ left.length
      }

    def visitEntry(i : Int) : Error[ModuleZipper] =
      focus match {
        case c : ContainerEntry =>
          if (i < 0 || i > c.entries.length - 1)
            fail("No entry at index " ++ i.toString)
          else {

            val (left, rightPlus) = c.entries.splitAt(i)
            val node = c match { case m : Module => m.moduleNode ; case i : Import => i.importNode }

            success(ModuleZipper(rightPlus.head, ModuleContext(node, left, rightPlus.tail) :: context))
          }
        case _ => fail("Focused entry is not a container")
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

    def insertAfter(entry : ModuleEntry) : Error[ModuleZipper] =
      context match {
        case Nil => fail("Cannot insert: no module open")
        case ModuleContext(node, left, right) :: cs =>
          success(ModuleZipper(entry, ModuleContext(node, left :+ focus, right) :: cs))

      }

    def insertBefore(entry : ModuleEntry) : Error[ModuleZipper] =
      context match {
        case Nil => fail("Cannot insert: no module open")
        case ModuleContext(node, left, right) :: cs =>
          success(ModuleZipper(entry, ModuleContext(node, left, focus +: right) :: cs))

      }

    // // Warning! The semantics of this method are a bit different from the two preceding it.
    // def insertAt(entry : ModuleEntry, i : Int) : Error[ModuleZipper] =
    //   focus match {
    //     case m : Module => {
    //       val (left, right) = m.entries.splitAt(i)

    //       // Now, we should have the insertion routine pass to all of the
    //       // *right* entries and update their addresses.

    //       // Then we can pass the module itself directly to an insertion routine
    //       // on the module description so that all of the stateful updates are
    //       // hidden by this purely functional routine.

    //       m.node.insertEntryAt(entry, i)
          
    //       val zip = ModuleZipper(m.copy(entries = left ++ Vector(entry) ++ right), context)

    //       // Now, for the values in the correct range, update all the addresses...

    //       for {
    //         k <- Range(left.length, m.entries.length + 1)
    //         child <- zip.visitEntry(k)
    //       } {
    //         child.foreachWithAddress {
    //           case (addr, node) => {
    //             node.address = addr
    //           }
    //         }
    //       }

    //       success(zip)
    //     }
    //     case _ => fail("Focused entry cannot have children")
    //   }
    
    // def appendEntry(entry : ModuleEntry) : Error[ModuleZipper] =
    //   focus match {
    //     case m : Module =>
    //       success(ModuleZipper(m.copy(entries = m.entries :+ entry), context))
    //     case _ => fail("Focused entry cannot have children.")
    //   }

    // def preprendEntry(entry : ModuleEntry) : Error[ModuleZipper] =
    //   focus match {
    //     case m : Module =>
    //       success(ModuleZipper(m.copy(entries = entry +: m.entries), context))
    //     case _ => fail("Focused entry cannot have children.")
    //   }

    // //   def firstChild : Error[ModuleZipper] = {
    // //     focus match {
    // //       case m : Module =>
    // //         if (m.entries.length > 0) {
    // //           ???
    // //         } else fail("Focused entry does not have children.")
    // //       case _ => fail("Focused entry does not have children.")
    // //     }
    // //   }

    // def foreachWithAddress(f : Function2[Vector[Int], NodeType, Unit]) : Unit =
    //   focus match {
    //     case m : Module => {
    //       for {
    //         i <- Range(0, m.entries.length)
    //       } {
    //         println("Visiting index " ++ i.toString ++ " on module " ++ m.node.name)
    //         visitEntry(i).get.foreachWithAddress(f)
    //       }

    //       f(toAddress, m.node)
    //     }
    //     case p : Parameter => f(toAddress, p.node)
    //     case d : Definition => f(toAddress, d.node)
    //     case i : Import => {
    //       // This is not right, but we will have to think about it as the semantics improve
    //       f(toAddress, i.node)
    //     }
    //   }

    // def focusedModule : Error[Module] =
    //   focus match {
    //     case m : Module => success(m)
    //     case _ => fail("Focused entry is not a module.")
    //   }
  }


  // case class Import(val importNode : ImportNode) extends ContainerEntry {

  //   def entries : Vector[ModuleEntry] = importNode.module.entries map (importEntry(_))

  //   def importEntry(entry : ModuleEntry) : ModuleEntry = 
  //     entry match {
  //       case m : Module => Module(new ImportedModuleNode(m), m.entries map (importEntry(_)))
  //       case i : Import => Import(new ImportedImportNode(i))
  //       case p : Parameter => Parameter(new ImportedParameterNode(p))
  //       case d : Definition => Definition(new ImportedDefinitionNode(d))
  //     }

  //   sealed trait ImportedNode extends Node

  //   class ImportedModuleNode(val module : Module) 
  //       extends ModuleNode(module.moduleNode.moduleName) 
  //       with ImportedNode

  //   class ImportedImportNode(val imprt : Import) 
  //       extends ImportNode(imprt.importNode.importName, imprt.importNode.module, imprt.importNode.isOpen) 
  //       with ImportedNode

  //   class ImportedParameterNode(val parameter : Parameter) 
  //       extends ParameterNode(parameter.parameterNode.variable)
  //       with ImportedNode

  //   class ImportedDefinitionNode(val definition : Definition) 
  //       extends DefinitionNode(definition.definitionNode.filler)
  //       with ImportedNode

  // }


}
