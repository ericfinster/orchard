/**
  * LiftPanel.scala - Base trait for HTML5 Panels using Lift
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.lift

import xml._
import orchard.core.ui._

trait LiftPanel[A] extends RenderingPanel[A] {

  override type CellType <: LiftCell
  override type EdgeType <: LiftEdge

  // We need a method which will return just the labels, and one which will 
  // return the whole rendered SVG.

  def labelProofSheet : NodeSeq

  abstract class LiftCell extends VisualCell { thisCell : CellType =>
  }

  abstract class LiftEdge extends VisualEdge { thisEdge : EdgeType =>
  }

}
