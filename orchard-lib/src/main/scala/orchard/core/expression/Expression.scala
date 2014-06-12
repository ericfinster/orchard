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

case class Reference(defn : ModuleSystem#Definition, addr : CellAddress) extends Expression {

  assert(defn.isComplete)

  def referencedExpression : Expression = 
    defn.filler.get.ncell.seek(addr).get.value

  def ident: Identifier = referencedExpression.ident
  def isThin: Boolean = referencedExpression.isThin

  // Perhaps some efficiency danger lurks here ...
  def ncell: NCell[Expression] = {
    val framework = new SimpleFramework(referencedExpression)
    framework.topCell.skeleton map (cell => Reference(defn, cell.address))
  }

  def styleString: String = referencedExpression.styleString

}

case class Substitution(expr : Expression, bindings : Map[Int, Expression]) extends Expression {

  def ident: Identifier = {
    Identifier(
      expr.ident.tokens map {
        case et @ ExpressionToken(Variable(_, idx, _, _)) =>
          if (bindings.isDefinedAt(idx)) {
            ExpressionToken(bindings(idx))
          } else et
        case tok @ _ => tok
      }
    )
  }

  def ncell: NCell[Expression] = 
    expr.ncell map (Substitution(_, bindings))

  // This we can't tell until we reduce, I think ...
  def isThin: Boolean = ???

  // Right, this too.  We have to reduce the guy to find out what he 
  // really will be.  Actually, no, I guess the only case that is difficult
  // is the boundary case.  And same for is thin.  So you should do these
  // guys after you have real reduction.
  def styleString: String = 
    expr match {
      case v : Variable(_, idx, _, _) => 
        if (bindings.isDefinedAt(idx))
          bindings(idx).styleString
        else
          v.styleString
      case _ => expr.styleString  // I think this will be wrong.
    }

}
