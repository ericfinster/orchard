/**
  * ExpressionMarker.scala - Marker's to put on expression cells
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.typechecker

import orchard.core.ui._

sealed trait ExpressionMarker extends Styleable {

  def isEmpty : Boolean
  def isShell : Boolean
  def isExposedNook : Boolean
  def isSelectable : Boolean

}

sealed trait PolarityMarker extends ExpressionMarker {

  def styleString = "polarity"

  def isEmpty = false
  def isShell = false
  def isExposedNook = false
  def isSelectable = false

}

case object PositivePolarityMarker extends PolarityMarker { def name = "+" }
case object NegativePolarityMarker extends PolarityMarker { def name = "-" }

case class EmptyMarker(val isShell : Boolean, val isExposedNook : Boolean) extends ExpressionMarker {

  def name = "empty"
  def styleString = "empty"

  def isEmpty = true
  def isSelectable = true

}

case class ReferenceMarker(val name : String, val styleString : String) extends ExpressionMarker {

  def isEmpty = false
  def isShell = false
  def isExposedNook = false
  def isSelectable = true

}
