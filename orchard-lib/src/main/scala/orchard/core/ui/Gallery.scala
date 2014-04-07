/**
  * Gallery.scala - A Gallery of Panels
  * 
  * @author Eric Finster
  * @version 0.1
  */

package orchard.core.ui

import orchard.core.util.EventConduit
import orchard.core.complex.CellEvent

trait Gallery[A] extends EventConduit[CellEvent] {

  type PanelType <: RenderingPanel[A]
  type GalleryCell = PanelType#CellType

  def complex : PanelType#ComplexType
  def panels : List[PanelType]

  def apply(idx : Int) : PanelType = panels(idx)
  def dimension : Int = panels.length - 1

  def renderAll = panels foreach (_.render)
  def refreshAll = panels foreach (_.refresh)

  def forallCells(action : PanelType#CellType => Unit) = {
    panels foreach (panel => {
      panel.baseCell foreachCell (cell => {
        action(cell)
      })
    })
  }

  def forallEdges(action : PanelType#EdgeType => Unit) = {
    panels foreach (panel => {
      for { tgt <- panel.baseCell.target } {
        tgt foreachEdge (edge => action(edge))
      }
    })
  }

}
