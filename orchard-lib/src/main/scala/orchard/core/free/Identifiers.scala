/**
  * Identifiers.scala - Identifiers
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.free

import scala.util.parsing.combinator.RegexParsers

import scalaz._
import Kleisli._
import MonadUtils._
import scalaz.std.list._
import scalaz.syntax.traverse._

trait Identifiers { thisChecker : TypeChecker =>

  //============================================================================================
  // IDENTIFIERS
  //

  case class Identifier(val tokens : List[IdentifierToken]) {

    def expand : InScope[String] = 
      for {
        expandedTokens <- (tokens map (_.expand)).sequence
      } yield expandedTokens.mkString

    override def toString = tokens.toString

  }

  object Identifier {

    def apply(tokens : IdentifierToken*) : Identifier = 
      Identifier(tokens.toList)

  }

  sealed trait IdentifierToken {

    def expand : InScope[String]

  }

  case class LiteralToken(val literal : String) extends IdentifierToken {

    def expand : InScope[String] = succeedInScope(literal)

    override def toString : String = "Lit(" ++ literal ++ ")"

  }

  case class ReferenceToken(val key : EnvironmentKey) extends IdentifierToken {

    def expand : InScope[String] = 
      for {
        expr <- lookup(key)
        exprName <- expr.name
      } yield exprName

    override def toString : String = "Ref(" ++ key.toString ++ ")"

  }


  //============================================================================================
  // RAW IDENTIFIERS
  //

  case class RawIdentifier(val tokens : List[RawIdentifierToken]) {
    override def toString = (tokens map (_.value)).mkString
  }

  sealed trait RawIdentifierToken { def value : String }
  case class RawLiteralToken(val value : String) extends RawIdentifierToken
  case class RawReferenceToken(val value : String) extends RawIdentifierToken

  abstract class IdentifierParser extends RegexParsers {

    def cleanIdentifierString : Parser[String] = """[^\${}/]+""".r

    def qualifiedPrefix : Parser[List[String]] = rep( cleanIdentifierString <~ "/" )
    def qualifiedName : Parser[String] = (qualifiedPrefix ^^ { (ls : List[String]) => ls.mkString }) <~ cleanIdentifierString

    def literal : Parser[RawLiteralToken] = cleanIdentifierString ^^ { tok => RawLiteralToken(tok) }
    def reference : Parser[RawReferenceToken] = "${" ~> cleanIdentifierString <~ "}" ^^ { tok => RawReferenceToken(tok) }

    def identifier : Parser[RawIdentifier] = rep( literal | reference ) ^^ { toks => RawIdentifier(toks) }

    override def skipWhitespace = false

  }

  object IdentParser extends IdentifierParser {

    def apply(input : String) = parseAll(identifier, input)

  }

  object ModuleIdentParser extends IdentifierParser {

    def apply(input : String) = parseAll(cleanIdentifierString, input)

  }

  // trait IdentifierModule {

  //   def processRawIdentifier(scope : Scope, rawIdent : RawIdentifier) : CheckerResult[Identifier] = {
  //     val idents : List[CheckerResult[IdentifierToken]] =
  //       rawIdent.tokens map {
  //         case RawLiteralToken(lit) => CheckerResult(LiteralToken(lit))
  //         case RawReferenceToken(ref) =>
  //           for {
  //             resultRef <- lookupIdentifier(ref, scope)
  //           } yield ReferenceToken(resultRef)
  //       }

  //     for {
  //       newIdents <- sequence(idents)
  //     } yield Identifier(newIdents)
  //   }

  //   def identifierToRaw(ident : Identifier) : RawIdentifier = {
  //     val rawTokens = ident.tokens map {
  //       case LiteralToken(lit) => RawLiteralToken(lit)
  //       case ReferenceToken(ref) => RawReferenceToken(ref.qualifiedName)
  //     }

  //     RawIdentifier(rawTokens)
  //   }

  // }

}
