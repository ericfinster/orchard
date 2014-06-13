/**
  * Identifier.scala - Routines for identifiers
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

import scala.util.parsing.combinator.RegexParsers

sealed trait Identifier {

  def tokens : List[IdentToken]

  def exprRefs : List[Expression] = 
    tokens flatMap {
      case et : ExpressionToken => Some(et.expr)
      case _ => None
    }

  def rawStr = (tokens map (_.rawStr)).mkString

  def idString = (tokens map (_.value)).mkString

}

case class VariableIdentifier(val index : Int, val tokens : List[IdentToken]) extends Identifier { 
  override def toString = idString
}

case class ExpressionIdentifier(val tokens : List[IdentToken]) extends Identifier {
  override def toString = idString
}

sealed trait IdentToken { def value : String ; def rawStr : String }
case class LiteralToken(val lit : String) extends IdentToken { def value = lit ; def rawStr = lit }
case class ExpressionToken(val expr : Expression) extends IdentToken { def value = expr.ident.toString ; def rawStr = "${" ++ expr.ident.rawStr ++ "}" }

object Identifier {

  def empty : Identifier = ExpressionIdentifier(List.empty)

}

case class RawIdentifier(val tokens : List[RawIdentToken]) {
  override def toString = (tokens map (_.value)).mkString
}

sealed trait RawIdentToken { def value : String }
case class RawLiteral(val value : String) extends RawIdentToken
case class RawReference(val value : String) extends RawIdentToken

object IdentParser extends RegexParsers {

  def cleanString : Parser[String] = """[^\${}]+""".r

  def literal : Parser[RawLiteral] = cleanString ^^ { tok => RawLiteral(tok) }
  def variable : Parser[RawReference] = "${" ~> cleanString <~ "}" ^^ { tok => RawReference(tok) }

  def tokenSeq : Parser[RawIdentifier] = rep( literal | variable ) ^^ { toks => RawIdentifier(toks) }

  def apply(input : String) = parseAll(tokenSeq, input)

  override def skipWhitespace = false

}
