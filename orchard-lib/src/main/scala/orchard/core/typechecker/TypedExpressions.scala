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

    def environmentIndex : Int
    def qualifiedName : QualifiedName

    def name : String = qualifiedName.localName

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
        }

      } yield result

  }


  case class ModuleExpression(
    val environmentIndex : Int, 
    val qualifiedName : QualifiedName, 
    val moduleEnvironment : Environment
  ) extends TypedExpression {

    def contents : Vector[TypedExpression] =
      moduleEnvironment.expressions.drop(environmentIndex)

  }

  sealed trait CellExpression extends TypedExpression { 

    def isThin : Boolean 
    def ident : Identifier

    def ncell : NCell[CellExpression]

  }

  class Variable(
    val environmentIndex : Int,
    val qualifiedName : QualifiedName, 
    val ident : Identifier, 
    val shell : Shell, 
    val isThin : Boolean
  ) extends CellExpression {

    def ncell = shell.withFillingExpression(this)

    override def toString : String = "Var(" ++ qualifiedName.toString ++ ")"

  }

  // Right, so at some point you should switch these two.  All of the important
  // information applies to the boundary.  The filler is secondary, as a witness
  // to the definition of the cell

  class Filler(
    val bdryEnvironmentIndex : Int,
    val bdryQualifiedName : QualifiedName,
    val bdryIdentifier : Identifier, 
    val nook : Nook
  ) extends CellExpression { thisFiller =>

    def environmentIndex = bdryEnvironmentIndex + 1
    def qualifiedName = bdryQualifiedName mapLocal ("def-" ++ _)

    def ident = Identifier(LiteralToken("def-") :: bdryIdentifier.tokens)
    def isThin = true
    def ncell = nook.withFiller(this)

    def bdryAddress : CellAddress =
      nook.framework.topCell.boundaryAddress

    override def toString : String = "Filler(" ++ qualifiedName.toString ++ ")"

    trait BoundaryExpr extends CellExpression {

      def environmentIndex = bdryEnvironmentIndex
      def qualifiedName = bdryQualifiedName

      def ident = bdryIdentifier
      def isThin = nook.isThinBoundary
      def ncell = nook.withBoundary(this)

      def interior = thisFiller

    }

    object Boundary extends BoundaryExpr {

      override def toString = "Boundary(" ++ qualifiedName.toString ++ ")"

    }

  }

}
