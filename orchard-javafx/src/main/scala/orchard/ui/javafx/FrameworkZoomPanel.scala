/**
  * FrameworkZoomPanel.scala - A zooming implementation of a framework panel
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import orchard.core._

class FrameworkZoomPanel(val complex : SimpleFramework, baseIndex : Int) extends ZoomPanel[Option[Expression]] with FrameworkPanel { thisPanel =>

  type CellType = FrameworkZoomCell
  type EdgeType = FrameworkZoomEdge

  override type ComplexType = SimpleFramework

  class FrameworkZoomCell(owner : complex.CellType) extends FrameworkCell(owner)
  class FrameworkZoomEdge(owner : complex.CellType) extends FrameworkEdge(owner)

  def newCell(owner : complex.CellType) = { 
    val frameworkCell = new FrameworkZoomCell(owner)
    owner.registerPanelCell(thisPanel)(frameworkCell)
    frameworkCell
  }

  def newEdge(owner : complex.CellType) = {
    val frameworkEdge = new FrameworkZoomEdge(owner)
    owner.registerPanelEdge(thisPanel)(frameworkEdge)
    frameworkEdge
  }


  //============================================================================================
  // INITIALIZATION
  //

  var baseCell : FrameworkZoomCell = newCell(complex.baseCells(baseIndex))

  refreshPanelData
  initializeChildren

}
