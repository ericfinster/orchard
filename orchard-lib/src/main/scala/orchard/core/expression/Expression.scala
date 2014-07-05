/**
  * Expression.scala - Opetopic Expressions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

import orchard.core.ui.Styleable
import orchard.core.complex._

trait ExpressionModule { thisChecker : TypeChecker =>

  sealed trait Expression extends Styleable {

    def isThin : Boolean

  }

  case class Variable(val ident : Identifier, shell : Shell, val isThin : Boolean) extends Expression {

    def name = ident.expand
    def styleString = if (isThin) "variable-thin" else "variable"

  }

  case class Filler(bdryIdent : Identifier, nook : Nook) extends Expression {

    def name = "def-" ++ bdryIdent.expand
    def isThin = true
    def styleString = "filler"

    trait BoundaryExpr extends Expression {

      def name = bdryIdent.expand
      def styleString = if (isThin) "bdry-thin" else "bdry"
      def isThin = nook.isThinBoundary

    }

    object Boundary extends BoundaryExpr {

    }
  }

  case class Reference(entry : ExpressionEntry) extends Expression {

    def name : String = entry.name
    def qualifiedId : String = ???
    def isThin : Boolean = entry.isThin

    def styleString : String = entry.styleString

  }

  // sealed trait ExpressionType
  // case object VariableType extends ExpressionType
  // case object FillerType extends ExpressionType
  // case object BoundaryType extends ExpressionType

  class Nook(framework : Framework[Option[Expression]]) {
    def isThinBoundary : Boolean = framework.topCell.isThinBoundary
  }

  class Shell(framework : Framework[Option[Expression]])

  //============================================================================================
  // EXPRESSIONLIKE DEFINITIONS
  //

  trait ExpressionLike[A] {

    def empty : A

    def isThin(a : A) : Boolean
    def isEmpty(a : A) : Boolean

  }

  object ExpressionLike {

    implicit def optExprIsExpressionLike : ExpressionLike[Option[Expression]] =
      new ExpressionLike[Option[Expression]] {

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

        def empty : Polarity[Option[Expression]] = Neutral(None)

        def isEmpty(p : Polarity[Option[Expression]]) = p == Neutral(None)
        def isThin(p : Polarity[Option[Expression]]) =
          p match {
            case Neutral(Some(expr)) => expr.isThin
            case _ => false
          }
      }

  }


}
