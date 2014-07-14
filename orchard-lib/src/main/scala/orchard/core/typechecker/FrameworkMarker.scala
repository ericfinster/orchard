/**
  * FrameworkMarker.scala - Markers for frameworks
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.typechecker

import orchard.core.ui._

// Should the polarities be something like this???

sealed trait FrameworkMarker {
  def isEmpty : Boolean
}

case object Empty extends FrameworkMarker {
  def isEmpty = true
}

sealed trait ExpressionMarker extends FrameworkMarker with Styleable {

  def isEmpty = false

}


