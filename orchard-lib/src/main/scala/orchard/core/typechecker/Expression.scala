/**
  * Expression.scala - Expression for Orchard
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.typechecker

import orchard.core.cell._
import orchard.core.ui.Styleable

sealed trait Expression extends Styleable {

  def isThin : Boolean

}

case class Variable(val name : String, shell : NCell[Option[Expression]], val isThin : Boolean) extends Expression {

  def styleString = if (isThin) "variable-thin" else "variable"

}

case class Filler(val name : String, nook : NCell[Option[Expression]]) extends Expression {

  def styleString = "filler"

  def isThin = true

}

case class Reference(val name : String) extends Expression {

  def styleString = "app"

  def isThin = false

}


