/**
  * Identifier.scala - Routines for identifiers
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

import scala.util.parsing.combinator.RegexParsers

sealed trait IdentToken { def value : String }
case class LiteralToken(val lit : String) extends IdentToken { def value = lit }
case class ReferenceToken(val ident : String) extends IdentToken { def value = ident }

object IdentParser extends RegexParsers {

  def cleanString : Parser[String] = """[^\${}]+""".r

  def literal : Parser[LiteralToken] = cleanString ^^ { tok => LiteralToken(tok) }
  def variable : Parser[ReferenceToken] = "${" ~> cleanString <~ "}" ^^ { tok => ReferenceToken(tok) }

  def tokenSeq : Parser[Identifier] = rep( literal | variable ) ^^ { toks => Identifier(toks) }

  def apply(input : String) = parseAll(tokenSeq, input)

  override def skipWhitespace = false
}
