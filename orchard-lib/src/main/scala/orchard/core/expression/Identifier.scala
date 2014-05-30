/**
  * Identifier.scala - Routines for identifiers
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

import scala.util.parsing.combinator.RegexParsers

case class Identifier(val tokens : List[IdentToken]) {

  def exprRefs : List[Expression] = 
    (tokens filter (_.isInstanceOf[ExpressionToken])) map 
      (_.asInstanceOf[ExpressionToken].expr)

  def rawStr = (tokens map (_.rawStr)).mkString

  override def toString = (tokens map (_.value)).mkString
}

sealed trait IdentToken { def value : String ; def rawStr : String }
case class LiteralToken(val lit : String) extends IdentToken { def value = lit ; def rawStr = lit }
case class ExpressionToken(val expr : Expression) extends IdentToken { def value = expr.ident.toString ; def rawStr = "${" ++ expr.ident.rawStr ++ "}" }

object Identifier {

  def empty : Identifier = Identifier(List.empty)

}

// This is busted.  Now that we have a situation where names can repeat themselves,
// referring just to the name may not be unique enough.  We're going to have to do
// something else to have better name resolution ...

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
