/**
  * ExpressionLike.scala - ExpressionLike TypeClass
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.checker

import orchard.core.complex._

trait ExpressionLike[A] {

  type NonEmptyType

  def empty : A

  def isThin(a : A) : Boolean
  def isEmpty(a : A) : Boolean

}

object ExpressionLike {

  implicit def optExprIsExpressionLike : ExpressionLike[Option[Expression]] =
    new ExpressionLike[Option[Expression]] {

      type NonEmptyType = Expression

      def empty : Option[Expression] = None

      def isEmpty(exprOpt : Option[Expression]) = exprOpt == None
      def isThin(exprOpt : Option[Expression]) =
        exprOpt match {
          case None => false
          case Some(expr) => expr.isThin
        }

    }

  implicit def polarityIsExpressionLike : ExpressionLike[Polarity[Option[Expression]]] =
    new ExpressionLike[Polarity[Option[Expression]]] {

      type NonEmptyType = Expression

      def empty : Polarity[Option[Expression]] = Neutral(None)

      def isEmpty(p : Polarity[Option[Expression]]) = p == Neutral(None)
      def isThin(p : Polarity[Option[Expression]]) =
        p match {
          case Neutral(Some(expr)) => expr.isThin
          case _ => false
        }
    }

}
