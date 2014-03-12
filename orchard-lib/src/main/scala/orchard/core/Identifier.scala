/**
  * Identifier.scala - Routines for identifiers
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

import scala.collection.mutable.Map
import scala.util.parsing.combinator.RegexParsers

case class Identifier(val tokens : List[IdentToken]) {
  override def toString = (tokens map (_.value)).mkString
}

object Identifier {

  implicit class IdentOps(ident : Identifier) {

    def translateWithBindings(bindings : Map[String, Expression]) : Identifier = {

      println("Translating identifier: " ++ ident.toString)

      val idnt = Identifier(
        ident.tokens map {
          case ReferenceToken(ref) => {
            if (bindings.isDefinedAt(ref)) {
              println(ref ++ " -> " ++ bindings(ref).id)
              ReferenceToken(bindings(ref).id)
            } else {
              // throw new IllegalArgumentException("Undefined reference in identifier translation: " ++ ref)
              println("Skipping unknown identifier: " ++ ref)
              ReferenceToken(ref)
            }
          }
          case tok @ _ => tok
        }
      )

      idnt
    }

  }

}

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
