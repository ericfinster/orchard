/**
  * Identifier.scala - Routines for identifiers
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

import orchard.core.util.Util

trait IdentifierModule { thisModule : TypeChecker =>

  case class Identifier(val tokens : List[IdentifierToken]) {

    def expand : String = (tokens map (_.expand)).mkString

  }

  sealed trait IdentifierToken {
    def expand : String
  }

  case class LiteralToken(val literal : String) extends IdentifierToken {
    def expand : String = literal
    override def toString : String = "Lit(" ++ literal ++ ")"
  }

  case class ReferenceToken(ref : TypeChecker#IdentifierType) extends IdentifierToken {
    def expand = ref.name
    override def toString : String = "Ref(" ++ ref.name ++ ")"
  }

  def processRawIdentifier(scope : Scope, rawIdent : RawIdentifier) : CheckerResult[Identifier] = {
    val idents : List[CheckerResult[IdentifierToken]] = 
      rawIdent.tokens map {
        case RawLiteralToken(lit) => CheckerResult(LiteralToken(lit))
        case RawReferenceToken(ref) =>
          for {
            resultRef <- lookupIdentifier(ref, scope)
          } yield ReferenceToken(resultRef)
      }

    for {
      newIdents <- sequence(idents)
    } yield Identifier(newIdents)
  }

  def identifierToRaw(ident : Identifier) : RawIdentifier = {
    val rawTokens = ident.tokens map {
      case LiteralToken(lit) => RawLiteralToken(lit)
      case ReferenceToken(ref) => RawReferenceToken(ref.qualifiedName)
    }

    RawIdentifier(rawTokens)
  }

}
