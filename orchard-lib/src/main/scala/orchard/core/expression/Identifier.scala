/**
  * Identifier.scala - Routines for identifiers
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

import orchard.core.util.Util

import scala.util.parsing.combinator.RegexParsers

trait IdentifierModule { thisModule : TypeChecker =>

  sealed trait Identifier {

    def expand : String

  }

  case class LiteralIdentifier(val literal : String) extends Identifier {
    def expand : String = literal
    override def toString : String = "Lit(" ++ literal ++ ")"
  }

  case class ReferenceIdentifier(ref : TypeChecker#IdentifierType) extends Identifier {
    def expand = ref.name
  }

  case class CompoundIdentifier(val components : List[Identifier]) extends Identifier {
    def expand : String = (components map (_.expand)).mkString
    override def toString : String = (components map (_.toString)).toString
  }

  object Identifier {

    def empty : Identifier = CompoundIdentifier(List.empty)

  }

  case class RawIdentifier(val tokens : List[RawIdentToken]) {
    override def toString = (tokens map (_.value)).mkString
  }

  sealed trait RawIdentToken { def value : String }
  case class RawLiteral(val value : String) extends RawIdentToken
  case class RawReference(val value : String) extends RawIdentToken


  abstract class IdentifierParser extends RegexParsers {

    def cleanString : Parser[String] = """[^\${}/]+""".r

    def literal : Parser[RawLiteral] = cleanString ^^ { tok => RawLiteral(tok) }
    def variable : Parser[RawReference] = "${" ~> cleanString <~ "}" ^^ { tok => RawReference(tok) }

    def tokenSeq : Parser[RawIdentifier] = rep( literal | variable ) ^^ { toks => RawIdentifier(toks) }

    override def skipWhitespace = false

  }

  object IdentParser extends IdentifierParser {

    def apply(input : String) = parseAll(tokenSeq, input)

  }

  object ModuleIdentParser extends IdentifierParser {

    def apply(input : String) = parseAll(cleanString, input)

  }

}
