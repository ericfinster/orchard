/**
  * StaticFrameworkPanel.scala - A static implementation of a framework panel
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import orchard.core._

class StaticFrameworkPanel(val complex : SimpleFramework, baseIndex : Int) extends StaticPanel[Option[Expression]] with FrameworkPanel { thisPanel =>

  type CellType = StaticFrameworkCell
  type EdgeType = StaticFrameworkEdge

  override type ComplexType = SimpleFramework

  class StaticFrameworkCell(val owner : complex.CellType) extends FrameworkCell
  class StaticFrameworkEdge(val owner : complex.CellType) extends FrameworkEdge

  def newCell(owner : complex.CellType) = { 
    val frameworkCell = new StaticFrameworkCell(owner)
    owner.registerPanelCell(thisPanel)(frameworkCell)
    frameworkCell
  }

  def newEdge(owner : complex.CellType) = {
    val frameworkEdge = new StaticFrameworkEdge(owner)
    owner.registerPanelEdge(thisPanel)(frameworkEdge)
    frameworkEdge
  }

  //============================================================================================
  // INITIALIZATION
  //

  var baseCell : StaticFrameworkCell = newCell(complex.baseCells(baseIndex))

  refreshPanelData
  initializeChildren

}
