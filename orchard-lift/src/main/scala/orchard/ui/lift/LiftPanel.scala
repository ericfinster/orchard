/**
  * LiftPanel.scala - A Panel Implementation in Lift
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.lift

import orchard.core.svg._
import orchard.core.complex._

import xml._

class LiftPanel[A](val complex : SimpleMutableComplex[A], baseIndex : Int) extends SVGPanel[A] { thisPanel =>

  type CellType = LiftCell
  type EdgeType = LiftEdge

  type ComplexType = SimpleMutableComplex[A]

  class LiftCell(val owner : complex.SimpleMutableCell) extends SVGCell
  class LiftEdge(val owner : complex.SimpleMutableCell) extends SVGEdge {
    def renderPath = ()
  }

  def newCell(owner : complex.SimpleMutableCell) : LiftCell = {
    val liftCell = new LiftCell(owner)
    owner.registerPanelCell(thisPanel)(liftCell)
    liftCell
  }

  def newEdge(owner : complex.SimpleMutableCell) : LiftEdge = {
    val liftEdge = new LiftEdge(owner)
    owner.registerPanelEdge(thisPanel)(liftEdge)
    liftEdge
  }

  //============================================================================================
  // UI INITIALIZATION
  //

  var baseCell : LiftCell = newCell(complex.baseCells(baseIndex))

  refreshPanelData

}
