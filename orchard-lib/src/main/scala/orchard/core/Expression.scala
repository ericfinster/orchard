/**
  * Expression.scala - Simple opetopic expressions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

import scala.util.parsing.combinator.RegexParsers

sealed trait Expression { def id : String ; def isThin : Boolean }

case class Variable(val ident : Identifier, val isThin : Boolean) extends Expression { 
  override def toString = id

  def id : String = ident.toString
}

case class FillerFace(val ident : Identifier, val filler : String, val isThin : Boolean) extends Expression {
  override def toString = id

  def id : String = ident.toString
}

case class Filler(val ident : Identifier) extends Expression { 
  override def toString = id 

  def isThin = true 
  def id : String = ident.toString
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
