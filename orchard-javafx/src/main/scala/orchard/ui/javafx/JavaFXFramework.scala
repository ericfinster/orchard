/**
  * JavaFXFramework - JavaFX Framework panel and gallery implementations
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

trait JavaFXFrameworkEnv { thisEnv : JavaFXWorkspace =>

  class FrameworkPanel(val complex : ExpressionFramework, val baseIndex : Int) 
      extends ZoomPanel[Option[Expression]] { thisPanel =>

    type ComplexType = ExpressionFramework

    type CellType = FrameworkPanelCell
    type EdgeType = FrameworkPanelEdge

    def newCell(owner : complex.ExpressionFrameworkCell) : FrameworkPanelCell = {
      val cell = new FrameworkPanelCell(owner)
      owner.registerPanelCell(thisPanel)(cell)
      reactTo(cell)
      cell
    }
    
    def newEdge(owner : complex.ExpressionFrameworkCell) : FrameworkPanelEdge = {
      val edge = new FrameworkPanelEdge(owner)
      owner.registerPanelEdge(thisPanel)(edge)
      reactTo(edge)
      edge
    }

    class FrameworkPanelCell(val owner : complex.ExpressionFrameworkCell) extends JavaFXCell { thisCell : CellType =>

      def renderLabel : jfxs.Node = {
        val labelNode = 
          item match {
            case None => new Region { prefWidth = 10 ; prefHeight = 10 }
            case Some(expr) => new Text(expr.toString)
          }

        labelNode.layoutBounds onChange { thisPanel.refresh }
        pane.getChildren.setAll(labelNode)
        labelNode
      }

      def getStyleString =
        item match {
          case None => "empty"
          case Some(expr) => expr.styleString
        }

      renderCell

      override def onEventEmitted(ev : CellEvent) = {
        ev match {
          case complex.ChangeEvents.ItemChangedEvent(oldItem) => { renderCell ;  super.onEventEmitted(ev) }
          case CellEntered(cell) => owner.emitToFaces(RequestCellHovered) ; owner.emit(RequestEdgeHovered)
          case CellExited(cell) => owner.emitToFaces(RequestCellUnhovered) ; owner.emit(RequestEdgeUnhovered)
          case _ => super.onEventEmitted(ev)
        }
      }

    }

    class FrameworkPanelEdge(val owner : complex.ExpressionFrameworkCell) extends JavaFXEdge { thisEdge : EdgeType => }

    //============================================================================================
    // INITIALIZATION
    //

    var baseCell : FrameworkPanelCell = newCell(complex.baseCells(baseIndex))

    refreshPanelData
    initializeChildren

  }

  class FrameworkGallery(val complex : ExpressionFramework) extends SpinnerGallery[Option[Expression]] { thisGallery =>

    def this(seed : NCell[Option[Expression]]) = this(new ExpressionFramework(seed))

    type PanelType = FrameworkPanel

    def newPanel(i : Int) : FrameworkPanel = {
      val panel = new FrameworkPanel(complex, i)
      reactTo(panel)
      panel
    }

    initialize

  }

}
