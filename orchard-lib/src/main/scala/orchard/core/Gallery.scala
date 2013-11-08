/**
  * Gallery.scala - A Gallery of Panels
  * 
  * @author Eric Finster
  * @version 0.1
  */

package orchard.core

trait Gallery[A] extends EventConduit[CellEvent] {

  type PanelType <: RenderingPanel[A]

  def complex : PanelType#ComplexType
  def panels : List[PanelType]

  def apply(idx : Int) : PanelType = panels(idx)
  def dimension : Int = panels.length - 1

  def renderAll : Unit = {
    panels foreach (panel => panel.render)
  }

}
