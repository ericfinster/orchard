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

  //============================================================================================
  // DEFINITIONAL EQUALITY
  //

  def canEqual(other : Any) : Boolean = 
    other.isInstanceOf[Variable]

  override def equals(other : Any) : Boolean = 
    other match {
      case that : Variable =>
        (that canEqual this) && (that.shell == this.shell) && (that.index == this.index)
      case _ => false
    }

  override def hashCode : Int = 
    41 * (
      41 * (
        41 + shell.hashCode
      ) + index.hashCode
    )

  override def toString = "Variable(" ++ id ++ ")"
}

case class Filler(val nook : Nook, bdryIdent : Identifier) extends Expression { thisFiller =>

  val ident = Identifier(LiteralToken("def-") :: bdryIdent.tokens)
  def isThin : Boolean = true
  def styleString = "filler"

  val ncell : NCell[Expression] = 
    nook.withFiller(thisFiller)

  //============================================================================================
  // DEFINITIONAL EQUALITY
  //

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

  override def toString = "Filler(" ++ id ++ ")"

  //============================================================================================
  // BOUNDARY DEFINITION
  //

  trait BoundaryExpr extends Expression { thisBdry =>

    val ident = bdryIdent
    val interior = thisFiller
    def isThin = nook.isThinBoundary
    def styleString = if (isThin) "bdry-thin" else "bdry"

    val ncell : NCell[Expression] =
      nook.withBoundary(thisBdry)

    def canEqual(other : Any) : Boolean =
      other.isInstanceOf[Filler#BoundaryExpr]

  }

  case object Boundary extends BoundaryExpr {

    //============================================================================================
    // DEFINITIONAL EQUALITY
    //

    override def equals(other : Any) : Boolean =
      other match {
        case that : Filler#BoundaryExpr =>
          (that canEqual this) && (that.interior == this.interior)
        case _ => false
      }

    override def hashCode : Int =
      41 * ( 41 + interior.hashCode )

    override def toString = "Boundary(" ++ id ++ ")"

  }

}

// Okay, this can't simply be an expression.  It needs to keep the information
// about where it can be found.  Make a new class called QualifiedExpression which
// records this kind of information? Defined expression?

// Now, we're going to need projection of addresses in cell trees I think

case class Reference(expr : Expression) extends Expression {

  def ident: Identifier = ???
  def isThin: Boolean = ???
  def ncell: NCell[Expression] = ???
  def styleString: String = ???

}

case class Substitution(expr : Expression) extends Expression {

  def ident: Identifier = ???
  def isThin: Boolean = ???
  def ncell: NCell[Expression] = ???
  def styleString: String = ???

}
