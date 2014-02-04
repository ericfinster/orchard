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

case class Variable(val id : String, val isThin : Boolean) extends Expression { 
  override def toString = id 
}

case class Filler(val toks : List[IdentToken], val nook : NCell[Option[Expression]]) extends Expression { 
  override def toString = id 

  def isThin = true 
  def id : String = IdentToken.getId(toks)
}

case class FillerTarget(val toks : List[IdentToken], val nook : NCell[Option[Expression]], val isThin : Boolean) extends Expression {
  override def toString = id

  def id : String = IdentToken.getId(toks)
}

sealed trait IdentToken { def value : String }
case class LiteralToken(val lit : String) extends IdentToken { def value = lit }
case class VariableToken(val ident : String) extends IdentToken { def value = ident }

object IdentToken {
  def getId(toks : List[IdentToken]) : String = (toks map (_.value)).mkString
}

object IdentParser extends RegexParsers {

  def cleanString : Parser[String] = """[^\${}]+""".r

  def literal : Parser[LiteralToken] = cleanString ^^ { tok => LiteralToken(tok) }
  def variable : Parser[VariableToken] = "${" ~> cleanString <~ "}" ^^ { tok => VariableToken(tok) }

  def tokenSeq : Parser[List[IdentToken]] = rep( literal | variable )

  def apply(input : String) = parseAll(tokenSeq, input)

  override def skipWhitespace = false
}
