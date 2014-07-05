/**
  * Expression.scala - Opetopic Expressions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

import orchard.core.cell._
import orchard.core.complex._

import orchard.core.ui.Styleable

trait ExpressionModule { thisChecker : TypeChecker =>

  sealed trait Expression extends Styleable {

    def isThin : Boolean
    def ncell : NCell[Expression]

  }

  case class Variable(val ident : Identifier, shell : Shell, val isThin : Boolean) extends Expression {

    def name = ident.expand
    def ncell = shell.withFillingExpression(this)
    def styleString = if (isThin) "variable-thin" else "variable"

  }

  case class Filler(bdryIdent : Identifier, nook : Nook) extends Expression { thisFiller =>

    def name = "def-" ++ bdryIdent.expand
    def ncell = nook.withFiller(this)
    def isThin = true
    def styleString = "filler"

    def bdryAddress : CellAddress =
      nook.framework.topCell.boundaryAddress

    trait BoundaryExpr extends Expression {

      def name = bdryIdent.expand
      def isThin = nook.isThinBoundary
      def styleString = if (isThin) "bdry-thin" else "bdry"

      def interior = thisFiller
      def ncell = interior.ncell.seek(bdryAddress).get

    }

    object Boundary extends BoundaryExpr {

    }
  }

  case class Reference(entry : ExpressionEntry) extends Expression {

    def name = entry.name
    def ncell = entry.expression.ncell
    def qualifiedId : String = ???
    def isThin : Boolean = entry.isThin

    def styleString : String = entry.styleString

  }

  //============================================================================================
  // NOOKS
  //

  class Nook(val framework : Framework[Option[Expression]]) {

    assert(framework.topCell.isNook)

    val ncell : NCell[Option[Expression]] = framework.topCell.toNCell

    def map(f : Expression => Expression) : Nook = {
      val duplicate = framework.duplicate

      duplicate forAllCells (cell => {
        cell.item = cell.item map f
      })

      new Nook(duplicate)
    }

    // def normalize : Nook = {
    //   // val duplicate = framework.duplicate

    //   // duplicate forAllFaces (cell => {
    //   //   cell.item foreach (e => {
    //   //     cell.item = Some(e.normalize)
    //   //     cell.promoteFaces
    //   //   })
    //   // })

    //   // new Nook(duplicate)

    //   map (_.normalize)
    // }

    def isThinBoundary : Boolean =
      framework.topCell.isThinBoundary

    def withFiller(filler : Filler) : NCell[Expression] = {
      val frameworkCopy = framework.extract(framework.topCell)
      frameworkCopy.topCell.item = Some(filler)
      frameworkCopy.topCell.boundaryFace.item = Some(filler.Boundary)
      frameworkCopy.topCell.toNCell map (_.get)
    }

    def canEqual(other : Any) : Boolean =
      other.isInstanceOf[Nook]

    override def equals(other : Any) : Boolean =
      other match {
        case that : Nook =>
          (that canEqual this) && (that.ncell == this.ncell)
        case _ => false
      }

    override def hashCode : Int =
      41 * (41 + ncell.hashCode)

  }

  //============================================================================================
  // SHELLS
  //

  class Shell(val framework : Framework[Option[Expression]]) {

    assert(framework.topCell.isShell)

    val ncell : NCell[Option[Expression]] = framework.topCell.toNCell

    def map(f : Expression => Expression) : Shell = {
      val duplicate = framework.duplicate

      duplicate forAllCells (cell => {
        cell.item = cell.item map f
      })

      new Shell(duplicate)
    }

    // def normalize : Shell = {
    //   // val duplicate = framework.duplicate

    //   // duplicate forAllFaces (cell => {
    //   //   cell.item foreach (e => {
    //   //     cell.item = Some(e.normalize)
    //   //     cell.promoteFaces
    //   //   })
    //   // })

    //   // new Shell(duplicate)

    //   map (_.normalize)
    // }

    def withFillingExpression(expr : Expression) : NCell[Expression] =
      framework.topCell.skeleton map (cell => {
        cell.item match {
          case None => expr
          case Some(e) => e
        }
      })

    def canEqual(other : Any) : Boolean =
      other.isInstanceOf[Shell]

    override def equals(other : Any) : Boolean =
      other match {
        case that : Shell =>
          (that canEqual this) && (that.ncell == this.ncell)
        case _ => false
      }

    override def hashCode : Int =
      41 * (41 + ncell.hashCode)

  }

  //============================================================================================
  // EXPRESSIONLIKE TYPECLASS
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
