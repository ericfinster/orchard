/**
  * GalleryComplex.scala - A complex which supports the spawning of panels
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.ui

// Erg.  I want to do something like this instead of the current setup.  But 
// I don't think I want to do it now.

trait GalleryComplex[A] extends MutableComplex[A] { thisComplex =>

  // type PanelType <: RenderingPanel[A]
  // type GalleryCell = PanelType#CellType

  // def panels : List[PanelType]

  // def apply(idx : Int) : PanelType = panels(idx)
  // def dimension : Int = panels.length - 1

  // def renderAll = panels foreach (_.render)
  // def refreshAll = panels foreach (_.refresh)

  // def forallCells(action : PanelType#CellType => Unit) = {
  //   panels foreach (panel => {
  //     panel.baseCell foreachCell (cell => {
  //       action(cell)
  //     })
  //   })
  // }

  // def forallEdges(action : PanelType#EdgeType => Unit) = {
  //   panels foreach (panel => {
  //     for { tgt <- panel.baseCell.target } {
  //       tgt foreachEdge (edge => action(edge))
  //     }
  //   })
  // }

}
