/**
  * Expression.scala - Opetopic Expressions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.checker

import orchard.core.cell._

sealed trait Expression {

  def name : String
  def styleString : String
  def isThin : Boolean
  def ncell : NCell[Expression]

}

case class Variable(val ident : Identifier, val shell : Shell, val isThin : Boolean) extends Expression {

  def name = ident.expand
  def ncell = ??? // shell.withFillingExpression(this)
  def styleString = if (isThin) "variable-thin" else "variable"

  def canEqual(other : Any) : Boolean =
    other.isInstanceOf[Variable]

  override def equals(other : Any) : Boolean =
    other match {
      case that : Variable =>
        (that canEqual this) &&
        (that.shell == this.shell) &&
        (that.ident.expand == this.ident.expand)
      case _ => false
    }

  override def hashCode : Int =
    41 * (
      41 * (
        41 + shell.hashCode
      ) + ident.expand.hashCode
    )

  override def toString : String = "Var(" ++ name ++ ")"

}

case class Filler(val bdryIdent : Identifier, val nook : Nook) extends Expression { thisFiller =>

  def name = "def-" ++ bdryIdent.expand
  def ncell = ??? // nook.withFiller(this)
  def isThin = true
  def styleString = "filler"

  // def bdryAddress : CellAddress =
  //   nook.framework.topCell.boundaryAddress

  def canEqual(other : Any) : Boolean =
    other.isInstanceOf[Filler]

  override def equals(other : Any) : Boolean =
    other match {
      case that : Filler =>
        (that canEqual this) && (that.nook == this.nook)
      case _ => false
    }

  override def hashCode : Int =
    41 * ( 41 + nook.hashCode )

  override def toString : String = "Filler(" ++ name ++ ")"

  trait BoundaryExpr extends Expression {

    def name = bdryIdent.expand
    def isThin = nook.isThinBoundary
    def styleString = if (isThin) "bdry-thin" else "bdry"

    def interior = thisFiller
    def ncell = ??? // interior.ncell.seek(bdryAddress).get

    def canEqual(other : Any) : Boolean =
      other.isInstanceOf[Filler#BoundaryExpr]

  }

  object Boundary extends BoundaryExpr {

    override def equals(other : Any) : Boolean =
      other match {
        case that : Filler#BoundaryExpr =>
          (that canEqual this) && (that.interior == this.interior)
        case _ => false
      }

    override def hashCode : Int =
      41 * ( 41 + interior.hashCode )

    override def toString = "Boundary(" ++ name ++ ")"
  }
}

case class Reference(val identifier : String) extends Expression {

  val name = identifier // entry.name
  def ncell = ??? /// entry.referenceNCell
  def isThin : Boolean = ??? // entry.isThin
  def styleString : String = ??? // entry.styleString

  def canEqual(other : Any) : Boolean =
    other.isInstanceOf[Reference]

  override def equals(other : Any) : Boolean =
    other match {
      case that : Reference =>
        (that canEqual this) &&
        (that.identifier == this.identifier)
      case _ => false
    }

  override def hashCode : Int =
    41 * ( 41 + this.identifier.hashCode)

  override def toString : String =
    "Ref(" ++ name ++ ")"

}
