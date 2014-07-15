/**
  * Expression.scala - Expression for Orchard
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.typechecker

import orchard.core.cell._

sealed trait Expression {

  def isThin : Boolean

}

case class Variable(name : String, shell : NCell[Option[Expression]], val isThin : Boolean) extends Expression 
case class Filler(name : String, nook : NCell[Option[Expression]]) extends Expression {

  def isThin = true

}

case class Reference(name : String) extends Expression {

  def isThin = ???

}


