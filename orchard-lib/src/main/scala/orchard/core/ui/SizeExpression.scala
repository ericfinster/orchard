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

}

case class Constant[A](val value : Double) extends SizeExpression[A] 
case class Plus[A](e : SizeExpression[A], f : SizeExpression[A]) extends SizeExpression[A]
case class Minus[A](e : SizeExpression[A], f : SizeExpression[A]) extends SizeExpression[A]
case class Max[A](e : SizeExpression[A], f : SizeExpression[A]) extends SizeExpression[A]
case class Divide[A](e : SizeExpression[A], f : SizeExpression[A]) extends SizeExpression[A]
case class Times[A](e : SizeExpression[A], f : SizeExpression[A]) extends SizeExpression[A]
case class If[A](cond : SizeCondition[A], e : SizeExpression[A], f : SizeExpression[A]) extends SizeExpression[A]

case class Attribute[A](ref : A, attr : String) extends SizeExpression[A]

sealed trait SizeCondition[A]

case class Gt[A](e : SizeExpression[A], f : SizeExpression[A]) extends SizeCondition[A]
case class Gte[A](e : SizeExpression[A], f : SizeExpression[A]) extends SizeCondition[A]
case class Lt[A](e : SizeExpression[A], f : SizeExpression[A]) extends SizeCondition[A]
case class Lte[A](e : SizeExpression[A], f : SizeExpression[A]) extends SizeCondition[A]
case class Eq[A](e : SizeExpression[A], f : SizeExpression[A]) extends SizeCondition[A]

object SizeExpression {

  implicit def doubleToSizeExpr[A](d : Double) : SizeExpression[A] =
    Constant(d)

}
