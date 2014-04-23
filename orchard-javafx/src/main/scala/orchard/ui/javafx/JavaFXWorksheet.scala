/**
  * JavaFXWorksheet.scala - Panel and gallery implementation for worksheets
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.scene.text.Text
import scalafx.scene.layout.Region

import orchard.core.ui._
import orchard.core.cell._
import orchard.core.editor._
import orchard.core.complex._
import orchard.core.expression._

import javafx.{scene => jfxs}

trait JavaFXWorksheetEnv { thisEnv : JavaFXWorkspace =>

  class WorksheetPanel(val complex : Worksheet, val baseIndex : Int) 
      extends ZoomPanel[Polarity[Option[Expression]]] { thisPanel =>

    type ComplexType = Worksheet

    type CellType = WorksheetPanelCell
    type EdgeType = WorksheetPanelEdge

    def newCell(owner : complex.WorksheetCell) : WorksheetPanelCell = {
      val cell = new WorksheetPanelCell(owner)
      owner.registerPanelCell(thisPanel)(cell)
      reactTo(cell)
      cell
    }
    
    def newEdge(owner : complex.WorksheetCell) : WorksheetPanelEdge = {
      val edge = new WorksheetPanelEdge(owner)
      owner.registerPanelEdge(thisPanel)(edge)
      reactTo(edge)
      edge
    }

    class WorksheetPanelCell(val owner : complex.WorksheetCell) extends JavaFXCell { thisCell : CellType =>

      def renderLabel : jfxs.Node = {
        val labelNode = 
          item match {
            case Positive => new Text("+")
            case Negative => new Text("-")
            case Neutral(None) => new Region { prefWidth = 10 ; prefHeight = 10 }
            case Neutral(Some(expr)) => new Text(expr.id)
          }

        labelNode.layoutBounds onChange { thisPanel.refresh }
        pane.getChildren.setAll(labelNode)
        labelNode
      }

      def getStyleString =
        item match {
          case Positive => "polarized"
          case Negative => "polarized"
          case Neutral(None) => if (owner.isFillable) "exposed" else "empty"
          case Neutral(Some(expr)) => expr.styleString
        }

      renderCell

      override def onEventEmitted(ev : CellEvent) = {
        ev match {
          case complex.ChangeEvents.ItemChangedEvent(oldItem) => { renderCell ;  super.onEventEmitted(ev) }
          case CellEntered(cell) => if (owner.isPolarized) () else { owner.emitToFaces(RequestCellHovered) ; owner.emit(RequestEdgeHovered) }
          case CellExited(cell) => if (owner.isPolarized) () else { owner.emitToFaces(RequestCellUnhovered) ; owner.emit(RequestEdgeUnhovered) }
          case _ => super.onEventEmitted(ev)
        }
      }

    }

    class WorksheetPanelEdge(val owner : complex.WorksheetCell) extends JavaFXEdge { thisEdge : EdgeType => }

    //============================================================================================
    // INITIALIZATION
    //

    var baseCell : WorksheetPanelCell = newCell(complex.baseCells(baseIndex))

    refreshPanelData
    initializeChildren

  }

  class WorksheetGallery(val complex : Worksheet) extends SpinnerGallery[Polarity[Option[Expression]]] { thisGallery =>

    def this(seed : NCell[Polarity[Option[Expression]]]) = this(new Worksheet(seed))

    type PanelType = WorksheetPanel

    def newPanel(i : Int) : WorksheetPanel = {
      val panel = new WorksheetPanel(complex, i)
      reactTo(panel)
      panel
    }

    initialize

    override def refreshAll = {
      super.refreshAll
      panels foreach (panel => {
        panel.baseCell foreachCell (cell => cell.assignStyle)
      })
    }

    //============================================================================================
    // EVENTS
    //

    override def onEventEmitted(ev : CellEvent) = {
      val cmplx = complex

      ev match {

        case PanelClicked => {
          cmplx.deselectAll
        }

        case ComplexExtended => {
          this(cmplx.dimension - 1).refresh
          val extPanel = newPanel(cmplx.dimension)
          appendPanel(extPanel)
          extPanel.render
          fastForward
        }

        case CellClicked(c) => {
          val cell = c.owner.asInstanceOf[cmplx.CellType]

          if (cell.isNeutral) {
            cmplx.clearAndSelect(cell)
          } else {
            cmplx.deselectAll
          }
        }

        case CellCtrlClicked(c) => {
          val cell = c.owner.asInstanceOf[cmplx.CellType]

          cmplx.selectionBase match {
            case None => if (cell.isNeutral) cmplx.selectAsBase(cell)
            case Some(base) => {
              if (cell != base) {
                if (cell.isPolarized) {
                  cmplx.deselectAll
                } else {
                  if (! cmplx.trySelect(cell)) cmplx.clearAndSelect(cell)
                }
              }
            }
          }
        }

        // This is clearly overkill, but the nooks need to be recalculated ...
        case cmplx.ChangeEvents.ItemChangedEvent(oldItem) => {
          refreshAll
        }

        case cmplx.ChangeEvents.CompositeInsertionEvent(c, u) => {
          val dim = c.dimension

          val compPanel = thisGallery(dim)
          val univPanel = thisGallery(dim + 1)

          val affectedDimensions = Range(dim, cmplx.dimension + 1)

          affectedDimensions foreach (i => panels(i).refresh)
        }

        case _ => super.onEventEmitted(ev)
      }
    }
  }
}
