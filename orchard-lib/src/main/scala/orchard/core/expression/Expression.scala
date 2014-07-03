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

    def name = ident.toString
    def styleString = if (isThin) "variable-thin" else "variable"

  }

  case class Filler(bdryIdent : Identifier, nook : Nook) extends Expression {

    def name = "def-" ++ bdryIdent.toString
    def isThin = true
    def styleString = "filler"

  }

  case class Reference(qualId : String, exprType : ExpressionType, isThin : Boolean) extends Expression {

    def name = qualId

    def styleString : String =
      exprType match {
        case VariableType => if (isThin) "variable-thin" else "variable"
        case FillerType => "filler"
        case BoundaryType => if (isThin) "bdry-thin" else "bdry"
      }

  }

  sealed trait ExpressionType
  case object VariableType extends ExpressionType
  case object FillerType extends ExpressionType
  case object BoundaryType extends ExpressionType

  class Nook(framework : Framework[Option[Expression]])
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
