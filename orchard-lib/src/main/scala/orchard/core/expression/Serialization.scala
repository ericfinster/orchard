/**
  * Serialization.scala - Serialization Routines
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

import scala.xml._
import java.io.File

import orchard.core.util._
import orchard.core.cell._

trait SerializationModule { thisModule : TypeChecker =>

  def statementToXML(stmt : Statement) : CheckerResult[Node] = 
    stmt match {
      case ModuleDefinition(name, body) => 
        for {
          bodyXML <- seqSeq(body map (statementToXML(_)))
        } yield {
          <module name={name}>{ bodyXML }</module>
        }
      case ModuleImport(_, _, _) => CheckerFailure("Module import serialization not implemented")
      case ParameterDefinition(variable) =>
        for {
          variableXML <- expressionToXML(variable)
        } yield {
          <parameter>{variableXML}</parameter>
        }
      case LiftDefinition(filler) => 
        for {
          fillerXML <- expressionToXML(filler)
        } yield {
          <lift>{fillerXML}</lift>
        }
    }

  def expressionToXML(expr : Expression) : CheckerResult[Node] = 
    expr match {
      case Variable(ident, shell, isThin) => {
        for {
          identXML <- identifierToXML(ident)
          shellXML <- shellToXML(shell)
        } yield {
          <variable isThin={isThin.toString}>{identXML ++ shellXML}</variable>
        }
      }
      case Filler(bdryIdent, nook) => {
        for {
          identXML <- identifierToXML(bdryIdent)
          nookXML <- nookToXML(nook)
        } yield {
          <filler>{identXML ++ nookXML}</filler>
        }
      }
      // Not quite sure what to do here ....
      case _ : Filler#BoundaryExpr => CheckerFailure("Attempt to serialize a boundary ...")
      case Reference(entry) => CheckerSuccess(<reference>{ entry.qualifiedName }</reference>)
    }

  def expressionFromXML(node : Node) : CheckerResult[Expression] = ???

  def identifierToXML(ident : Identifier) : CheckerResult[Node] = 
    ident match {
      case LiteralIdentifier(lit) => CheckerResult(<literal>{lit}</literal>)
      case ReferenceIdentifier(ref) => CheckerResult(<reference>{ ref.qualifiedName }</reference>)
      case CompoundIdentifier(components) => 
        for {
          componentsXML <- sequence (components map (identifierToXML(_)))
        } yield {
          <identifier>{ componentsXML }</identifier>
        }
    }

  def nookToXML(nook : Nook) : CheckerResult[Node] = {
    val frameworkSerializer = implicitly[XmlSerializable[NCell[Option[Expression]]]]
    val nookComplexXML = frameworkSerializer.toXML(nook.framework.topCell.toNCell)
    CheckerSuccess(<nook>{ nookComplexXML }</nook>)
  }

  def shellToXML(shell : Shell) : CheckerResult[Node] = {
    val frameworkSerializer = implicitly[XmlSerializable[NCell[Option[Expression]]]]
    val shellComplexXML = frameworkSerializer.toXML(shell.framework.topCell.toNCell)
    CheckerSuccess(<shell>{ shellComplexXML }</shell>)
  }

  def writeToFile(file : File) : CheckerResult[Unit] =
    for {
      moduleXML <- statementToXML(rootModule.toRawSyntax)
    } yield {
      XML.save(file.getAbsolutePath, moduleXML)
    }

  // def loadModule(file : java.io.File) = {
  //   val elem = xml.XML.loadFile(file.getAbsolutePath)

  //   elem match {
  //     case m @ <module>{defns @ _*}</module> => {
  //       val name = (m \ "@name").text
  //       val moduleItem = 
  //         new TreeItem[ModuleTreeItem] {
  //           value = ModuleItem(name)
  //         }

  //       defns foreach (defn => {
  //         addDefinitionToModule(Definition.fromXML(defn), moduleItem)
  //       })    

  //       moduleRoot.children += moduleItem
  //     }
  //   }
  // }
  
  //============================================================================================
  // SERIALIZATION INSTANCES
  //

  implicit def expressionSerializable : XmlSerializable[Expression] = 
    new XmlSerializable[Expression] {

      def toXML(expr : Expression) : Node =
        expressionToXML(expr) match {
          case CheckerSuccess(node) => node
          case _ => throw new RuntimeException("Serialization failed")
        }

      def fromXML(node : Node) : Expression = ???

    }

  implicit def ncellOptExprSerializable : XmlSerializable[NCell[Option[Expression]]] = 
    new XmlSerializable[NCell[Option[Expression]]] {

      val cellSerializer = implicitly[XmlSerializable[Cell[_ <: Nat, Option[Expression]]]]

      def toXML(ncell : NCell[Option[Expression]]) : Node = {
        cellSerializer.toXML(ncell.cell)
      }

      def fromXML(node : Node) : NCell[Option[Expression]] = {
        cellSerializer.fromXML(node)
      }
    }

}
