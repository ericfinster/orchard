/**
  * ModuleEnties.scala - Module entries and zippers
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.free

import orchard.core.util.ErrorM.{succeed => succeedE, fail => failE, _}

trait ModuleEntries { thisChecker : TypeChecker =>

  //============================================================================================
  // MODULE ENTRIES
  //

  sealed trait ModuleEntry { def node : Node }

  sealed trait ContainerEntry extends ModuleEntry { def entries : Vector[ModuleEntry] }
  sealed trait ExpressionEntry extends ModuleEntry { def expression : Expression }

  case class Module(val moduleNode : ModuleNode, val entries : Vector[ModuleEntry]) extends ContainerEntry {
    def node = moduleNode
  }

  case class Import(val importNode : ImportNode, val entries : Vector[ModuleEntry]) extends ContainerEntry {
    def node = importNode
  }

  case class Parameter(val parameterNode : ParameterNode) extends ExpressionEntry { 
    def node = parameterNode 
    def expression = parameterNode.variable
  }

  case class Definition(val definitionNode : DefinitionNode) extends ExpressionEntry { 
    def node = definitionNode 
    def expression = definitionNode.filler
  }

  //============================================================================================
  // NODES
  //

  sealed trait Node { 
    def name : String = qualifiedName.localName
    def qualifiedName : QualifiedName = LocalName("Unknown")
    override def toString = qualifiedName.toString
  }

  sealed trait ContainerNode extends Node
  sealed trait ExpressionNode extends Node

  class ModuleNode(override val qualifiedName : QualifiedName, val worksheets : Vector[Worksheet]) extends ContainerNode {
    def moduleName = name
  }

  class ImportNode extends ContainerNode
  class ParameterNode(override val qualifiedName : QualifiedName, val variable : Variable) extends ExpressionNode
  class DefinitionNode(override val qualifiedName : QualifiedName, val filler : Filler) extends ExpressionNode {

    val fillerQualifiedName = qualifiedName mapLocal ("def-" ++ _)

  }

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
        case Nil => failE("Zip failed: emtpy context.")
        case ModuleContext(node, left, right) :: cs =>
          succeedE(ModuleZipper(seal(node, left ++ Vector(focus) ++ right), cs))
      }

    def zip : ModuleEntry =
      parent match {
        case Left(_) => focus
        case Right(p) => p.zip
      }

    def withFocus(entry : ModuleEntry) : ModuleZipper =
      ModuleZipper(entry, context)

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
        case Nil => failE("Cannot move left in empty context")
        case ModuleContext(node, left, right) :: cs => {
          if (left.length < 1) {
            failE("No left sibling")
          } else {
            succeedE(ModuleZipper(left.last, ModuleContext(node, left.init, focus +: right) :: cs))
          }
        }
      }

    def uncle : Error[ModuleZipper] = 
      for {
        p <- parent
        l <- p.leftSibling
      } yield l

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
            failE("No entry at index " ++ i.toString)
          else {

            val (left, rightPlus) = c.entries.splitAt(i)
            val node = c match { case m : Module => m.moduleNode ; case i : Import => i.importNode }

            succeedE(ModuleZipper(rightPlus.head, ModuleContext(node, left, rightPlus.tail) :: context))
          }
        case _ => failE("Focused entry is not a container")
      }

    def seek(addr : Vector[Int]) : Error[ModuleZipper] =
      if (addr.length <= 0) {
        succeedE(this)
      } else {
        for {
          entry <- visitEntry(addr.head)
          res <- entry.seek(addr.tail)
        } yield res
      }

    def insertAfter(entry : ModuleEntry) : Error[ModuleZipper] =
      context match {
        case Nil => failE("Cannot insert: no module open")
        case ModuleContext(node, left, right) :: cs =>
          succeedE(ModuleZipper(entry, ModuleContext(node, left :+ focus, right) :: cs))

      }

    def insertBefore(entry : ModuleEntry) : Error[ModuleZipper] =
      context match {
        case Nil => failE("Cannot insert: no module open")
        case ModuleContext(node, left, right) :: cs =>
          succeedE(ModuleZipper(entry, ModuleContext(node, left, focus +: right) :: cs))

      }

    def focusAsModule : Error[Module] = 
      focus match {
        case m : Module => succeedE(m)
        case _ => failE("Focused entry is not a module")
      }

    def appendToModule(entry : ModuleEntry) : Error[ModuleZipper] = 
      focus match {
        case m : Module => {
          succeedE(ModuleZipper(entry, ModuleContext(m.moduleNode, m.entries, Vector.empty) :: context))
        }
        case _ => failE("Focused entry is not a module")
      }

  }

}
