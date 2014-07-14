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
      case ParameterDefinition(rawIdent, shell, isThin) => 
        for {
          identXML <- rawIdentifierToXML(rawIdent)
          shellXML <- ncellToXML(shell)
        } yield {
          <parameter isThin={isThin.toString}>{ identXML ++ shellXML }</parameter>
        }
      case LiftDefinition(rawBdryIdent, nook) => 
        for {
          bdryIdentXML <- rawIdentifierToXML(rawBdryIdent)
          nookXML <- ncellToXML(nook)
        } yield {
          <lift>{ bdryIdentXML ++ nookXML }</lift>
        }
    }

  def statementFromXML(node : Node) : CheckerResult[Statement] = 
    node match {
      case <module>{ moduleContents @ _* }</module> => 
        for {
          body <- seqSeq(moduleContents map (statementFromXML(_)))
        } yield {
          val moduleName = (node \ "@name").text
          ModuleDefinition(moduleName, body.toList)
        }
      case <parameter>{ variableXML }</parameter> => 
        for {
          ident <- rawIdentifierFromXML(variableXML(0))
          shell <- ncellFromXML[Option[String]](variableXML(1))
        } yield {
          val isThin = (node \ "@isThin").text.toBoolean
          ParameterDefinition(ident, shell, isThin)
        }
      case <lift>{ fillerXML }</lift> =>
        for {
          ident <- rawIdentifierFromXML(fillerXML(0))
          nook <- ncellFromXML[Option[String]](fillerXML(1))
        } yield {
          LiftDefinition(ident, nook)
        }
    }

  def rawIdentifierToXML(ident : RawIdentifier) : CheckerResult[Node] = {
    def tokenToXML(tok : RawIdentifierToken) : Node = 
      tok match {
        case RawLiteralToken(lit) => <literal>{lit}</literal>
        case RawReferenceToken(qualifiedName) => <reference>{ qualifiedName }</reference>
      }

    CheckerResult(<identifier>{ ident.tokens map tokenToXML }</identifier>)
  }

  def rawIdentifierFromXML(node : Node) : CheckerResult[RawIdentifier] = {
    def tokenFromXML(n : Node) : RawIdentifierToken = 
      n match {
        case <literal>{ contents }</literal> => RawLiteralToken(contents.text)
        case <reference>{ contents }</reference> => RawReferenceToken(contents.text)
      }

    node match {
      case <identifier>{ identContents }</identifier> => {
        val rawTokens = identContents map tokenFromXML
        CheckerResult(RawIdentifier(rawTokens.toList))
      }
    }
  }

  def ncellToXML[A : XmlSerializable](ncell : NCell[A]) : CheckerResult[Node] = {
    val ncellSerializer = implicitly[XmlSerializable[NCell[A]]]
    CheckerSuccess(<ncell>{ ncellSerializer.toXML(ncell) }</ncell>)
  }

  def ncellFromXML[A : XmlSerializable](node : Node) : CheckerResult[NCell[A]] = {
    val ncellSerializer = implicitly[XmlSerializable[NCell[A]]]
    CheckerResult(ncellSerializer.fromXML(node))
  }

  def writeToFile(file : File) : CheckerResult[Unit] =
    for {
      moduleXML <- statementToXML(rootModule.toRawSyntax)
    } yield {
      editor.consoleDebug("Writing data: " ++ rootModule.toRawSyntax.toString)
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

}
