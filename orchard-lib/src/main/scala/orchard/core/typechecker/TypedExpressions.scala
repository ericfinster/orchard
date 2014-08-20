/**
  * TypedExpressions.scala - Typed expressions are the result of the typechecker
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.typechecker

import orchard.core.cell._
import orchard.core.util._

import ErrorM._
import MonadUtils._

trait TypedExpressions { thisChecker : TypeChecker =>

  import CheckerErrorSyntax._

  sealed trait TypedExpression {

    def name : String = qualifiedName.localName
    def qualifiedName : QualifiedName

  }

  case class ModuleExpression(
    val qualifiedName : QualifiedName, 
    val contents : Vector[TypedExpression]
  ) extends TypedExpression

  sealed trait CellExpression extends TypedExpression {

    def isThin : Boolean

  }

  case class Variable(
    val qualifiedName : QualifiedName, 
    val ident : Identifier, 
    val shell : Shell, 
    val isThin : Boolean
  ) extends CellExpression {

    def canEqual(other : Any) : Boolean =
      other.isInstanceOf[Variable]

    override def equals(other : Any) : Boolean =
      other match {
        case that : Variable =>
          (that canEqual this) &&
          (that.shell == this.shell) &&
          (that.ident == this.ident)
        case _ => false
      }

    override def hashCode : Int =
      41 * (
        41 * (
          41 + shell.hashCode
        ) + ident.expand.hashCode
      )

     override def toString : String = "Var(" ++ qualifiedName.toString ++ ")"

  }

  case class Filler(
    val bdryQualifiedName : QualifiedName,
    val bdryIdentifier : Identifier, 
    val bdryIsThin : Boolean,
    val nook : Nook
  ) extends CellExpression { thisFiller =>

    def qualifiedName = bdryQualifiedName mapLocal ("def-" ++ _)
    def ident = Identifier(LiteralToken("def-") :: bdryIdentifier.tokens)
    def isThin = true

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

    override def toString : String = "Filler(" ++ qualifiedName.toString ++ ")"

    trait BoundaryExpr extends CellExpression {

      def qualifiedName = bdryQualifiedName
      def ident = bdryIdentifier
      def isThin = bdryIsThin

      def interior = thisFiller

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

      override def toString = "Boundary(" ++ qualifiedName.toString ++ ")"

    }

  }

  case class Reference(
    val qualifiedName : QualifiedName, 
    val index : Int, 
    val isThin : Boolean
  ) extends CellExpression {

    def canEqual(other : Any) : Boolean =
      other.isInstanceOf[Reference]

    override def equals(other : Any) : Boolean =
      other match {
        case that : Reference =>
          (that canEqual this) &&
          (that.index == this.index)
        case _ => false
      }

    override def hashCode : Int =
      41 * ( 41 + this.index.hashCode)

    override def toString : String =
      "Ref(" ++ index.toString ++ " : " ++ qualifiedName.toString ++ ")"

  }

}
