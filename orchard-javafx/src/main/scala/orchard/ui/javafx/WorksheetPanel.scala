/**
  * WorksheetPanel.scala - A Panel implementation for Worksheets
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.Includes._
import scalafx.scene.text.Text
import scalafx.scene.layout.Region

import javafx.{scene => jfxs}

import orchard.core.complex._
import orchard.core.typechecker._

class WorksheetPanel(val worksheet : WorksheetHandle, val baseIndex : Int) 
    extends ZoomPanel[ExpressionMarker] { thisPanel =>

  type ComplexType = MarkerComplex

  val complex = worksheet.complex

  type CellType = WorksheetPanelCell
  type EdgeType = WorksheetPanelEdge

  override def newCell(owner : complex.CellType) : CellType = {
    val cell = new WorksheetPanelCell(owner)
    owner.registerPanelCell(thisPanel)(cell)
    reactTo(cell)
    cell
  }

  override def newEdge(owner : complex.CellType) : EdgeType = {
    val edge = new WorksheetPanelEdge(owner)
    owner.registerPanelEdge(thisPanel)(edge)
    reactTo(edge)
    edge
  }

  class WorksheetPanelCell(val owner : complex.CellType) extends JavaFXCell { thisCell : CellType =>

    def renderLabel : jfxs.Node = {
      val labelNode = 
        if (owner.item.isEmpty) {
          new Region { prefWidth = 10 ; prefHeight = 10 }
        } else {
          new Text(owner.item.name)
        }

      labelNode.layoutBounds onChange { thisPanel.refresh }
      pane.getChildren.setAll(labelNode)
      labelNode
    }

    def getStyleString = 
      owner.item.styleString

    renderCell

    override def onEventEmitted(ev : CellEvent) = {
      ev match {
        // case complex.ChangeEvents.ItemChangedEvent(oldItem) => { renderCell ;  super.onEventEmitted(ev) }
        case CellEntered(cell) => owner.emitToFaces(RequestCellHovered) ; owner.emit(RequestEdgeHovered)
        case CellExited(cell) => owner.emitToFaces(RequestCellUnhovered) ; owner.emit(RequestEdgeUnhovered)
        case _ => super.onEventEmitted(ev)
      }
    }

  }

  class WorksheetPanelEdge(val owner : complex.CellType) extends JavaFXEdge { thisEdge : EdgeType => }

  //============================================================================================
  // INITIALIZATION
  //

  var baseCell : WorksheetPanelCell = newCell(complex.baseCells(baseIndex))

  refreshPanelData
  initializeChildren

}
