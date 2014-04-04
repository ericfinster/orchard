/**
  * WorksheetPanel.scala - A panel implementation for worksheets
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.scene.text.Text
import scalafx.scene.layout.Region

import orchard.core._

import javafx.{scene => jfxs}

trait WorksheetEnvironment { thisEnvironment : JavaFXWorkspace =>

  abstract class WorksheetPanel
      extends ZoomPanel[Polarity[IndexType]]
      with MutablePanel[Polarity[IndexType]] { thisPanel =>

    type CellType <: WorksheetPanelCell
    type EdgeType <: WorksheetPanelEdge

    type ComplexType <: Worksheet

    abstract class WorksheetPanelCell extends JavaFXCell with MutablePanelCell { thisCell : CellType =>

      def expression = owner.expression

      def renderLabel : jfxs.Node = {
        val labelNode = 
          if (owner.isPolarized) {
            new Text(owner.item.toString)
          } else {
            expression match {
              case None => new Region { prefWidth = 10 ; prefHeight = 10 }
              case Some(expr) => new Text(thisEnvironment.context.expandIdentifier(expr.ident))
            }
          }

        labelNode.layoutBounds onChange { thisPanel.refresh }
        pane.getChildren.setAll(labelNode)
        labelNode
      }

      def getStyleString =
        if (owner.isPolarized) {
          "polarized"
        } else {
          expression match {
            case None => "empty"
            case Some(expr) => expr.styleString
          }
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

    abstract class WorksheetPanelEdge extends JavaFXEdge with MutablePanelEdge { thisEdge : EdgeType => }

  }

  abstract class WorksheetGallery extends SpinnerGallery[Polarity[IndexType]] { thisGallery =>

    //def this(seed : NCell[Polarity[IndexType]]) = this(new Worksheet(seed))

    type PanelType <: WorksheetPanel

    // def newPanel(i : Int) : WorksheetPanel = {
    //   val panel = new WorksheetPanel(complex, i)
    //   reactTo(panel)
    //   panel
    // }

    // initialize

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
