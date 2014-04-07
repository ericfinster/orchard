/**
  * JavaFXWorkspace.scala - JavaFX implementation of a workspace
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

class JavaFXWorkspace(
  val editor : JavaFXEditor,
  val name : String,
  val stabilityLevel : Option[Int],
  val invertibilityLevel : Option[Int],
  val unicityLevel : Option[Int]
) extends Workspace {

  var activeSheet : Option[Worksheet] = None
  var activeExpression : Option[NCell[Expression]] = None

  def newSheet = ()

  class WorksheetPanel extends ZoomPanel[Polarity[Option[Expression]]] { thisPanel =>

    type ComplexType = Worksheet

    type CellType = WorksheetPanelCell
    type EdgeType = WorksheetPanelEdge

    class WorksheetPanelCell extends JavaFXCell { thisCell : CellType =>

      def renderLabel : jfxs.Node = {
        val labelNode = 
          item match {
            case Positive => new Text("+")
            case Negative => new Text("-")
            case Neutral(None) => new Region { prefWidth = 10 ; prefHeight = 10 }
            case Neutral(Some(expr)) => new Text(expr.toString)
          }

        labelNode.layoutBounds onChange { thisPanel.refresh }
        pane.getChildren.setAll(labelNode)
        labelNode
      }

      def getStyleString =
        item match {
          case Positive => "polarized"
          case Negative => "polarized"
          case Neutral(None) => "empty"
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

    class WorksheetPanelEdge extends JavaFXEdge { thisEdge : EdgeType => }

  }

  // abstract class WorksheetGallery extends SpinnerGallery[Polarity[IndexType]] { thisGallery =>

  //   //def this(seed : NCell[Polarity[IndexType]]) = this(new Worksheet(seed))

  //   type PanelType <: WorksheetPanel

  //   // def newPanel(i : Int) : WorksheetPanel = {
  //   //   val panel = new WorksheetPanel(complex, i)
  //   //   reactTo(panel)
  //   //   panel
  //   // }

  //   // initialize

  //   //============================================================================================
  //   // EVENTS
  //   //

  //   override def onEventEmitted(ev : CellEvent) = {
  //     val cmplx = complex

  //     ev match {

  //       case PanelClicked => {
  //         cmplx.deselectAll
  //       }

  //       case ComplexExtended => {
  //         this(cmplx.dimension - 1).refresh
  //         val extPanel = newPanel(cmplx.dimension)
  //         appendPanel(extPanel)
  //         extPanel.render
  //         fastForward
  //       }

  //       case CellClicked(c) => {
  //         val cell = c.owner.asInstanceOf[cmplx.CellType]

  //         if (cell.isNeutral) {
  //           cmplx.clearAndSelect(cell)
  //         } else {
  //           cmplx.deselectAll
  //         }
  //       }

  //       case CellCtrlClicked(c) => {
  //         val cell = c.owner.asInstanceOf[cmplx.CellType]

  //         cmplx.selectionBase match {
  //           case None => if (cell.isNeutral) cmplx.selectAsBase(cell)
  //           case Some(base) => {
  //             if (cell != base) {
  //               if (cell.isPolarized) {
  //                 cmplx.deselectAll
  //               } else {
  //                 if (! cmplx.trySelect(cell)) cmplx.clearAndSelect(cell)
  //               }
  //             }
  //           }
  //         }
  //       }

  //       // This is clearly overkill, but the nooks need to be recalculated ...
  //       case cmplx.ChangeEvents.ItemChangedEvent(oldItem) => {
  //         refreshAll
  //       }

  //       case cmplx.ChangeEvents.CompositeInsertionEvent(c, u) => {
  //         val dim = c.dimension

  //         val compPanel = thisGallery(dim)
  //         val univPanel = thisGallery(dim + 1)

  //         val affectedDimensions = Range(dim, cmplx.dimension + 1)

  //         affectedDimensions foreach (i => panels(i).refresh)
  //       }

  //       case _ => super.onEventEmitted(ev)
  //     }
  //   }
  // }
}
