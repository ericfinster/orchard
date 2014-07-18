/**
  * SimpleSVGPanel.scala - Simple implementation of an SVG Panel
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.svg

import orchard.core.complex._

import xml._

class SimpleSVGPanel[A](val complex : SimpleMutableComplex[A], baseIndex : Int) extends SVGPanel[A] { thisPanel =>

  type CellType = SimpleSVGCell
  type EdgeType = SimpleSVGEdge

  type ComplexType = SimpleMutableComplex[A]

  class SimpleSVGCell(val owner : complex.SimpleMutableCell) extends SVGCell
  class SimpleSVGEdge(val owner : complex.SimpleMutableCell) extends SVGEdge {
    def renderPath = ()
  }

  def newCell(owner : complex.SimpleMutableCell) : SimpleSVGCell = {
    val simpleCell = new SimpleSVGCell(owner)
    owner.registerPanelCell(thisPanel)(simpleCell)
    simpleCell
  }

  def newEdge(owner : complex.SimpleMutableCell) : SimpleSVGEdge = {
    val simpleEdge = new SimpleSVGEdge(owner)
    owner.registerPanelEdge(thisPanel)(simpleEdge)
    simpleEdge
  }

  //============================================================================================
  // UI INITIALIZATION
  //

  var baseCell : SimpleSVGCell = newCell(complex.baseCells(baseIndex))

  refreshPanelData

}
