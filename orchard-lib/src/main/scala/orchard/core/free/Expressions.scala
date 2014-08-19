/**
  * Expressions.scala - Opetopic Expressions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.free

import orchard.core.cell._
import orchard.core.util._

import ErrorM._
import MonadUtils._

trait Expressions { thisChecker : TypeChecker => 

  sealed trait Expression {

    def name : InScope[String]
    def isThin : InScope[Boolean]
    def ncell : InScope[NCell[Expression]]

  }

  case class Variable(val ident : Identifier, val shell : Shell, val isThinVar : Boolean) extends Expression {

    def name = ident.expand
    def ncell = succeedInScope(shell.withFillingExpression(this))
    def isThin = succeedInScope(isThinVar)

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

    override def toString : String = "Var(" ++ ident.toString ++ ")"

  }

  case class Filler(val bdryIdent : Identifier, val nook : Nook) extends Expression { thisFiller =>

    def name = 
      for {
        boundaryName <- Boundary.name
      } yield "def-" ++ boundaryName

    def ncell = attemptInScope(nook.withFiller(this))
    def isThin = succeedInScope(true)

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
          boundaryNCell <- attemptInScope(
            fromOption(
              interiorNCell.seek(bdryAddress), 
              "Internal Error: could not find boundary"
            )
          )
        } yield boundaryNCell

      def isThin = nook.isThinBoundary

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
