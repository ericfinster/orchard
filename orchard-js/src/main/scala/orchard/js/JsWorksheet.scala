/**
  * JsWorksheet.scala - A worksheet implementation in javascript
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.js

import scala.scalajs._
import org.scalajs.dom
import dom.document
import org.scalajs.jquery.jQuery

import orchard.core.checker._

class JsWorksheet(
  val container : dom.Element,
  json : js.Any,
  val galleryPanelWidth : Int,
  val galleryPanelHeight : Int,
  val displayPanels : Int
) extends JsComplex[WorksheetMarker](json) {

  type CellType = JsWorksheetCell

  type PanelType = JsPanel

  type PanelCellType = JsWorksheetPanelCell
  type PanelEdgeType = JsWorksheetPanelEdge

  def newPanel(i : Int) : PanelType = new JsPanel(i)
  def newCell(item : WorksheetMarker) : JsWorksheetCell = 
    new JsWorksheetCell(item)

  class JsWorksheetCell(var item : WorksheetMarker) extends JsCell { thisCell =>

    object MyPanelCell extends JsWorksheetPanelCell {
      def complexCell = thisCell
    }

    object MyPanelEdge extends JsWorksheetPanelEdge {
      def complexCell = thisCell
    }

    def panelCell = MyPanelCell
    def panelEdge = MyPanelEdge

  }

  trait JsWorksheetPanelCell extends JsPanelCell {

    override def styleBase = "orchard-cell-" ++ complexCell.item.styleString

    override def drawLabel(p : Paper) = {
      val label = p.text(0, 0, item.name)
      label.attr(js.Dynamic.literal(("class" -> "orchard-label")))
      labelElement = Some(label)
      label
    }

    override def onMouseOver : Unit = {
      if (complexCell.item.isNeutral) {
        super.onMouseOver
      }
    }

    override def onMouseOut : Unit = {
      if (complexCell.item.isNeutral) {
        super.onMouseOut
      }
    }

    override def onMouseClick : Unit = {
      
      for { f <- complexCell.faces } { f.panelCell.requestSelectedStyle }
    }

  }

  trait JsWorksheetPanelEdge extends JsPanelEdge

}
