/**
  * Identifier.scala - Routines for identifiers
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

import scala.collection.mutable.Map
import scala.util.parsing.combinator.RegexParsers

case class IndexedIdentifier[A](val tokens : List[IndexedIdToken[A]]) {
  def map[B](f : A => B) = IndexedIdentifier(tokens map (_ map f))
}

sealed trait IndexedIdToken[A] {
  def map[B](f : A => B) : IndexedIdToken[B]
}

case class StringToken[A](str : String) extends IndexedIdToken[A] {
  def map[B](f : A => B) = StringToken[B](str)
}

case class IndexToken[A](idx : A) extends IndexedIdToken[A] {
  def map[B](f : A => B) = IndexToken[B](f(idx))
}

case class Identifier(val tokens : List[IdentToken]) {
  override def toString = (tokens map (_.value)).mkString
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
