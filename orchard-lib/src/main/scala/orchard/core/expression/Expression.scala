/**
  * Expression.scala - Opetopic Expressions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

import orchard.core.ui.Styleable

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

case class Reference(val name : String, exprType : ExpressionType, isThin : Boolean) extends Expression {

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
