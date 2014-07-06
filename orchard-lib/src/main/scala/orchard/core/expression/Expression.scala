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

  case class Variable(val ident : Identifier, val shell : Shell, val isThin : Boolean) extends Expression {

    def name = ident.expand
    def ncell = shell.withFillingExpression(this)
    def styleString = if (isThin) "variable-thin" else "variable"

    def canEqual(other : Any) : Boolean =
      other.isInstanceOf[Variable]

    override def equals(other : Any) : Boolean =
      other match {
        case that : Variable =>
          (that canEqual this) && 
          (that.shell == this.shell) &&
          (that.ident.expand == this.ident.expand)
        case _ => false
      }

    override def hashCode : Int =
      41 * (
        41 * (
          41 + shell.hashCode
        ) + ident.expand.hashCode
      )

  }

  case class Filler(val bdryIdent : Identifier, val nook : Nook) extends Expression { thisFiller =>

    def name = "def-" ++ bdryIdent.expand
    def ncell = nook.withFiller(this)
    def isThin = true
    def styleString = "filler"

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

    trait BoundaryExpr extends Expression {

      def name = bdryIdent.expand
      def isThin = nook.isThinBoundary
      def styleString = if (isThin) "bdry-thin" else "bdry"

      def interior = thisFiller
      def ncell = interior.ncell.seek(bdryAddress).get

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

    }
  }

  case class Reference(val entry : ExpressionEntry) extends Expression {

    val name = entry.name
    def ncell = entry.referenceNCell

    def qualifiedName : String = {
      println("Getting qualified name for: " ++ name)
      entry.qualifiedName
    }

    def isThin : Boolean = entry.isThin

    def styleString : String = entry.styleString

    def canEqual(other : Any) : Boolean =
      other.isInstanceOf[Reference]

    override def equals(other : Any) : Boolean =
      other match {
        case that : Reference =>
          (that canEqual this) && 
          (that.entry.qualifiedName == this.entry.qualifiedName)
        case _ => false
      }

    override def hashCode : Int =
      41 * ( 41 + this.entry.qualifiedName.hashCode)

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

    def withFiller(filler : Filler) : NCell[Expression] =
      withFillerAndBoundary(filler, filler.Boundary)

    def withFillerAndBoundary(filler : Expression, boundary : Expression) : NCell[Expression] = {
      val frameworkCopy = framework.duplicate
      frameworkCopy.topCell.item = Some(filler)
      frameworkCopy.topCell.boundaryFace.item = Some(boundary)
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
