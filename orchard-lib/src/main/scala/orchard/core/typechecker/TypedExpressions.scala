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

  object TypedExpression {

    // Let's try to make a pretty printer for the expressions ....

    import scalaz._
    import scalaz.syntax.traverse._
    import scalaz.std.vector._

    type ExprPrinter[+A] = Reader[String, A]

    val ExprPrinterReader = MonadReader[Reader, String]
    import ExprPrinterReader._

    def prettyPrint(exprs : Vector[TypedExpression]) : ExprPrinter[String] = 
      for {
        results <- sequence(
          exprs map ((e : TypedExpression) =>
            prettyPrint(e)
          )
        )
      } yield results.mkString("\n")

    def prettyPrint(expr : TypedExpression) : ExprPrinter[String] = 
      for {
        indent <- ask

        result <- expr match {
          case m : ModuleExpression => {
            for {
              contentStr <- scope(indent ++ " ")(prettyPrint(m.contents))
            } yield {
              indent ++ "Module: " ++ m.qualifiedName.toString ++ "\n" ++ contentStr
            }
          }

          case v : Variable => point(indent ++ v.toString)
          case f : Filler => point(indent ++ f.toString)
          case b : Filler#BoundaryExpr => point(indent ++ b.toString)
          case r : Reference => point(indent ++ r.toString)
        }

      } yield result

  }


  case class ModuleExpression(
    val qualifiedName : QualifiedName, 
    val contents : Vector[TypedExpression],
    val referenceCutoff : Int     //  Any reference in the module *strictly less* is external
  ) extends TypedExpression

  sealed trait CellExpression extends TypedExpression { def isThin : Boolean }
  sealed trait ConcreteCellExpression extends CellExpression { def ncell : NCell[CellExpression] }

  case class Variable(
    val qualifiedName : QualifiedName, 
    val ident : Identifier, 
    val shell : Shell, 
    val isThin : Boolean
  ) extends ConcreteCellExpression {

    def ncell = shell.withFillingExpression(this)

    def canEqual(other : Any) : Boolean =
      other.isInstanceOf[Variable]

    override def equals(other : Any) : Boolean =
      other match {
        case that : Variable =>
          (that canEqual this) &&
          (that.shell == this.shell) &&
          (that.qualifiedName.toString == this.qualifiedName.toString)
        case _ => false
      }

    override def hashCode : Int =
      41 * (
        41 * (
          41 + shell.hashCode
        ) + qualifiedName.toString.hashCode
      )

     override def toString : String = "Var(" ++ qualifiedName.toString ++ ")"

  }

  case class Filler(
    val bdryQualifiedName : QualifiedName,
    val bdryIdentifier : Identifier, 
    val nook : Nook
  ) extends ConcreteCellExpression { thisFiller =>

    def qualifiedName = bdryQualifiedName mapLocal ("def-" ++ _)
    def ident = Identifier(LiteralToken("def-") :: bdryIdentifier.tokens)
    def isThin = true
    def ncell = nook.withFiller(this)

    def bdryAddress : CellAddress =
      nook.framework.topCell.boundaryAddress

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

    trait BoundaryExpr extends ConcreteCellExpression {

      def qualifiedName = bdryQualifiedName
      def ident = bdryIdentifier
      def isThin = nook.isThinBoundary
      def ncell = nook.withBoundary(this)

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
