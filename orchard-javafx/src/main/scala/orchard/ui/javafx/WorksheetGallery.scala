/**
  * WorksheetGallery.scala - A Gallery for displaying Worksheets
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

class WorksheetGallery(val handle : WorksheetHandle) extends SpinnerGallery[ExpressionMarker] {

  type PanelType = WorksheetPanel

  var complex = handle.complex

  def newPanel(i : Int) : WorksheetPanel = {
    val panel = new WorksheetPanel(i)
    reactTo(panel)
    panel
  }

  def update : Unit = {
    complex = handle.complex
    initialize
    refreshAll
  }

  initialize

  //============================================================================================
  // EVENTS
  //

  override def onEventEmitted(ev : CellEvent) = {
    val cmplx = complex

    ev match {

      case PanelClicked => {
        cmplx.deselectAll
      }

      case CellClicked(c) => {
        val cell = c.owner.asInstanceOf[cmplx.CellType]

        if (cell.isNeutral) {
          cmplx.clearAndSelect(cell)
        } else {
          cmplx.deselectAll
        }
      }

      case CellDoubleClicked(c) => ()

      case CellCtrlClicked(c) => {
        val cell = c.owner.asInstanceOf[cmplx.CellType]

        cmplx.selectionBase match {
          case None => if (cell.isNeutral) cmplx.selectAsBase(cell)
          case Some(base) => {
            if (cell != base) {
              if (! cell.isNeutral) {
                cmplx.deselectAll
              } else {
                if (! cmplx.trySelect(cell)) cmplx.clearAndSelect(cell)
              }
            }
          }
        }
      }

      case _ => super.onEventEmitted(ev)
    }
  }

  class WorksheetPanel(val baseIndex : Int)
      extends ZoomPanel[ExpressionMarker] { thisPanel =>

    type ComplexType = handle.MarkerComplex

    val complex = handle.complex

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

}
