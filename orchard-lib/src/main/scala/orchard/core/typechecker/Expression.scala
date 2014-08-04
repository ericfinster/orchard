/**
  * Expression.scala - Opetopic Expressions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.typechecker

import orchard.core.cell._
import orchard.core.util._

import ErrorM._
import MonadUtils._

trait CheckerExpressions { thisChecker : Checker => 

  sealed trait Expression {

    def name : CheckerM[String]
    def isThin : CheckerM[Boolean]
    def ncell : CheckerM[NCell[Expression]]

  }

  case class Variable(val ident : Identifier, val shell : Shell, val isThinVar : Boolean) extends Expression {

    def name = ident.expand
    def ncell = checkerSucceed(shell.withFillingExpression(this))
    def isThin = checkerSucceed(isThinVar)

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

    // override def toString : String = "Var(" ++ name ++ ")"

  }

  case class Filler(val bdryIdent : Identifier, val nook : Nook) extends Expression { thisFiller =>

    def name = 
      for {
        boundaryName <- Boundary.name
      } yield "def-" ++ boundaryName

    def ncell = checkerSucceed(nook.withFiller(this))
    def isThin = checkerSucceed(true)

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

    // override def toString : String = "Filler(" ++ name ++ ")"

    trait BoundaryExpr extends Expression {

      def name = bdryIdent.expand

      def interior = thisFiller

      def ncell =
        for {
          interiorNCell <- interior.ncell
          boundaryNCell <- liftError(
            fromOption(
              interiorNCell.seek(bdryAddress), 
              "Internal Error: could not find boundary"
            )
          )
        } yield boundaryNCell

      def isThin = ??? //checkerSucceed(nook.isThinBoundary)


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

      // override def toString = "Boundary(" ++ name ++ ")"
    }
  }

  case class Reference(val key : EnvironmentKey) extends Expression {

    def name = 
      for {
        expr <- lookup(key)
        exprName <- expr.name
      } yield exprName

    def ncell = 
      for {
        expr <- lookup(key)
        exprNCell <- expr.ncell
      } yield exprNCell

    def isThin = 
      for {
        expr <- lookup(key)
        exprIsThin <- expr.isThin
      } yield exprIsThin

    def canEqual(other : Any) : Boolean =
      other.isInstanceOf[Reference]

    override def equals(other : Any) : Boolean =
      other match {
        case that : Reference =>
          (that canEqual this) &&
          (that.key == this.key)
        case _ => false
      }

    override def hashCode : Int =
      41 * ( 41 + this.key.hashCode)

    override def toString : String =
      "Ref(" ++ key.toString ++ ")"

  }

}
