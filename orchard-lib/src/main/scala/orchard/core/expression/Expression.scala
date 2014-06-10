/**
  * Expression.scala - Expression definitions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

import orchard.core.cell._

sealed trait Expression {

  def ident : Identifier
  def isThin : Boolean
  def styleString : String

  def id = ident.toString

  def ncell : NCell[Expression]

}

case class Variable(val shell : Shell, val index : Int, val ident : Identifier, val isThin : Boolean) extends Expression {

  def styleString = if (isThin) "var-thin" else "var"

  val ncell : NCell[Expression] =
    shell.withFillingExpression(this)

  override def toString = "Variable(" ++ id ++ ")"
}

case class Filler(val nook : Nook, bdryIdent : Identifier) extends Expression { thisFiller =>

  val ident = Identifier(LiteralToken("def-") :: bdryIdent.tokens)
  def isThin : Boolean = true
  def styleString = "filler"

  val ncell : NCell[Expression] = 
    nook.withFiller(thisFiller)

  override def toString = "Filler(" ++ id ++ ")"

  trait BoundaryExpr extends Expression { thisBdry =>

    val ident = bdryIdent
    val interior = thisFiller
    def isThin = nook.isThinBoundary
    def styleString = if (isThin) "bdry-thin" else "bdry"

    val ncell : NCell[Expression] =
      nook.withBoundary(thisBdry)

  }

  case object Boundary extends BoundaryExpr {
    override def toString = "Boundary(" ++ id ++ ")"
  }

}

