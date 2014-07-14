/**
  * RawSyntax.scala - Raw serializable syntax
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

import orchard.core.cell._

import scala.util.parsing.combinator.RegexParsers

trait SyntaxModule { thisModule : TypeChecker =>

  sealed trait Statement
  case class ModuleDefinition(name : String, body : List[Statement]) extends Statement
  case class ModuleImport(moduleIdent : String, moduleName : String, bindings : Map[String, String]) extends Statement
  case class ParameterDefinition(rawIdent : RawIdentifier, shell : NCell[Option[String]], isThin : Boolean) extends Statement
  case class LiftDefinition(rawBdryIdent : RawIdentifier, nook : NCell[Option[String]]) extends Statement

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

}



