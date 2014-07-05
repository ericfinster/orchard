/**
  * Identifier.scala - Routines for identifiers
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

import orchard.core.util.Util

import scala.util.parsing.combinator.RegexParsers

// The distinction between tokens and terms is annoying ...

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

case class VariableIdentifier(val index : Int, val varIdent : Identifier) extends Identifier {
  def expand = varIdent.expand
  override def toString : String = "Var(" ++ index.toString ++ ", " ++ varIdent.toString ++ ")"
}

abstract case class ClosureIdentifier(val body : Identifier) extends Identifier {

  def identMap : Map[Int, Identifier]

  def wrap(i : Identifier) : ClosureIdentifier

  // Below we seem to perform a rewrite, but not continue to reduce ...
  def reduce : Identifier = {

    if (Util.debug) {
      println(toString)
    }

    body match {
      case li : LiteralIdentifier => li
      case ci : CompoundIdentifier => 
        CompoundIdentifier(ci.components map (wrap(_).reduce))
      case vi : VariableIdentifier => {
        if (identMap.isDefinedAt(vi.index)) {
          identMap(vi.index)
        } else {
          VariableIdentifier(vi.index, wrap(vi.varIdent))
        }
      }
      case ci : ClosureIdentifier => {
        wrap(ci.reduce)
      }
      case _ => ???
    }
  }

  def expand = reduce.expand

  override def toString : String = "Closure(" ++ body.toString ++ ", " ++ identMap.mapValues(_.toString).toString ++ ")"

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
