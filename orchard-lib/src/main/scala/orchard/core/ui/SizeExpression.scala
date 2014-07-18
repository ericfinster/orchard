/**
  * SizeExpression.scala - A simple expression language for determining sizes
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.ui

import scala.language.implicitConversions

sealed trait SizeExpression[A] { thisExpr =>

  def +(expr : SizeExpression[A]) : SizeExpression[A] = Plus(thisExpr, expr)
  def -(expr : SizeExpression[A]) : SizeExpression[A] = Minus(thisExpr, expr)
  def *(expr : SizeExpression[A]) : SizeExpression[A] = Times(thisExpr, expr)
  def /(expr : SizeExpression[A]) : SizeExpression[A] = Divide(thisExpr, expr)

  def ===(expr : SizeExpression[A]) : SizeCondition[A] = Eq(thisExpr, expr)
  def <(expr : SizeExpression[A]) : SizeCondition[A] = Lt(thisExpr, expr)
  def <=(expr : SizeExpression[A]) : SizeCondition[A] = Lte(thisExpr, expr)
  def >(expr : SizeExpression[A]) : SizeCondition[A] = Gt(thisExpr, expr)
  def >=(expr : SizeExpression[A]) : SizeCondition[A] = Gte(thisExpr, expr)

  override def toString : String = 
    thisExpr match {
      case Constant(value) => value.toString
      case Plus(e, f) => "( " ++ e.toString ++ " + " ++ f.toString ++ " )"
      case Minus(e, f) => "( " ++ e.toString ++ " - " ++ f.toString ++ " )"
      case Max(e, f) => "Math.max( " ++ e.toString ++ ", " ++ f.toString ++ " )"
      case Divide(e, f) => "( " ++ e.toString ++ " / " ++ f.toString ++ " )"
      case Times(e, f) => "( " ++ e.toString ++ " * " ++ f.toString ++ " )"
      case If(cond, e, f) => "if (" ++ cond.toString ++ ") { " ++ e.toString ++ " } else { " ++ f.toString ++ " }"
      case Attribute(ref, attr) => "Ref(" ++ ref.toString ++ "." ++ attr ++ ")"
    }
}

case class Constant[A](val value : Double) extends SizeExpression[A] 
case class Plus[A](e : SizeExpression[A], f : SizeExpression[A]) extends SizeExpression[A]
case class Minus[A](e : SizeExpression[A], f : SizeExpression[A]) extends SizeExpression[A]
case class Max[A](e : SizeExpression[A], f : SizeExpression[A]) extends SizeExpression[A]
case class Divide[A](e : SizeExpression[A], f : SizeExpression[A]) extends SizeExpression[A]
case class Times[A](e : SizeExpression[A], f : SizeExpression[A]) extends SizeExpression[A]
case class If[A](cond : SizeCondition[A], e : SizeExpression[A], f : SizeExpression[A]) extends SizeExpression[A]

case class Attribute[A](ref : A, attr : String) extends SizeExpression[A]

sealed trait SizeCondition[A] { thisCond =>

  override def toString : String = 
    thisCond match {
      case Gt(e, f) => e.toString ++ " > " ++ f.toString
      case Gte(e, f) => e.toString ++ " >= " ++ f.toString
      case Lt(e, f) => e.toString ++ " < " ++ f.toString
      case Lte(e, f) => e.toString ++ " <= " ++ f.toString
      case Eq(e, f) => e.toString ++ " == " ++ f.toString
    }

}

case class Gt[A](e : SizeExpression[A], f : SizeExpression[A]) extends SizeCondition[A]
case class Gte[A](e : SizeExpression[A], f : SizeExpression[A]) extends SizeCondition[A]
case class Lt[A](e : SizeExpression[A], f : SizeExpression[A]) extends SizeCondition[A]
case class Lte[A](e : SizeExpression[A], f : SizeExpression[A]) extends SizeCondition[A]
case class Eq[A](e : SizeExpression[A], f : SizeExpression[A]) extends SizeCondition[A]

object SizeExpression {

  implicit def doubleToSizeExpr[A](d : Double) : SizeExpression[A] =
    Constant(d)

}
