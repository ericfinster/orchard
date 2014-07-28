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

import orchard.core.complex._
import orchard.core.checker._

class JsWorksheet(
  val container : dom.Element,
  json : js.Any,
  val galleryPanelWidth : Int,
  val galleryPanelHeight : Int,
  val displayPanels : Int
) extends JsComplex[WorksheetMarker](json) 
    with SelectableComplex[WorksheetMarker] {

  type CellType = JsWorksheetCell

  type PanelType = JsPanel

  type PanelCellType = JsWorksheetPanelCell
  type PanelEdgeType = JsWorksheetPanelEdge

  def newPanel(i : Int) : PanelType = new JsPanel(i)
  def newCell(item : WorksheetMarker) : JsWorksheetCell = 
    new JsWorksheetCell(item)

  override def select(cell : JsWorksheetCell) = {
    super.select(cell)
    for { f <- cell.faces } { f.panelCell.requestSelectedStyle }
  }

  override def deselect(cell : JsWorksheetCell) = {
    super.deselect(cell)
    for { f <- cell.faces } { f.panelCell.requestUnselectedStyle }
  }

  override def refreshFromJson(newJson : js.Any) : Unit = {
    deselectAll
    super.refreshFromJson(newJson)
    // clear all the selections.  Will this help?
  }


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
      import js.Dynamic.{literal => lit}

      item match {
        case e : EmptyMarker => {
          val markerRect = p.rect(0, 0, 10, 10)
          markerRect.attr(lit("class" -> "orchard-empty-label"))
          labelElement = Some(markerRect)
          markerRect
        }
        case _ => {
          val label = p.text(0, 0, item.name)
          label.attr(lit(("class" -> "orchard-label")))
          labelElement = Some(label)
          label
        }
      }
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

    override def onMouseClick(e : js.Any) : Unit = {

      import JsJsonReader._

      val isControl = readBoolean(readObjectField(e, "ctrlKey"))

      if (isControl) {
        selectionBase match {
          case None => if (complexCell.item.isNeutral) selectAsBase(complexCell)
          case Some(base) => {
            if (complexCell != base) {
              if (! complexCell.item.isNeutral) {
                deselectAll
              } else {
                if (! trySelect(complexCell)) clearAndSelect(complexCell)
              }
            }
          }
        }
      } else {
        if (complexCell.item.isNeutral) {
          clearAndSelect(complexCell)
        } else {
          deselectAll
        }
      }
    }

    // case CellClicked(c) => {
    //   val cell = c.owner.asInstanceOf[cmplx.CellType]

    //   if (cell.isNeutral) {
    //     cmplx.clearAndSelect(cell)
    //   } else {
    //     cmplx.deselectAll
    //   }
    // }

    // case CellCtrlClicked(c) => {
    //   val cell = c.owner.asInstanceOf[cmplx.CellType]

    //   cmplx.selectionBase match {
    //     case None => if (cell.isNeutral) cmplx.selectAsBase(cell)
    //     case Some(base) => {
    //       if (cell != base) {
    //         if (cell.isPolarized) {
    //           cmplx.deselectAll
    //         } else {
    //           if (! cmplx.trySelect(cell)) cmplx.clearAndSelect(cell)
    //         }
    //       }
    //     }
    //   }
    // }

  }

  trait JsWorksheetPanelEdge extends JsPanelEdge

}
