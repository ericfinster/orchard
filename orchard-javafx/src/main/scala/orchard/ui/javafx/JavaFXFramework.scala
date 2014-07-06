/**
  * JavaFXFramework.scala - Panel and Gallery Implementation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.Includes._

import scalafx.scene.text.Text
import scalafx.scene.layout.Region

import orchard.core.ui._
import orchard.core.cell._
import orchard.core.complex._
import orchard.core.expression._

import javafx.{scene => jfxs}

trait JavaFXFrameworkModule { thisModule : JavaFXTypeCheckerMixin =>

  //============================================================================================
  // GALLERY IMPLEMENTATION
  //

  class FrameworkGallery(val complex : Framework[Option[Expression]]) extends SpinnerGallery[Option[Expression]] { thisGallery =>

    // def this(seed : NCell[Option[Expression]]) = this(new WorkspaceFramework(seed))
    // def this(expr : Expression) = this(expr.ncell map (Some(_)))

    type PanelType = FrameworkPanel

    def newPanel(i : Int) : FrameworkPanel = {
      val panel = new FrameworkPanel(complex, i)
      reactTo(panel)
      panel
    }

    //============================================================================================
    // EVENTS
    //

    override def onEventEmitted(ev : CellEvent) = {
      val cmplx = complex

      ev match {
        case CellClicked(c) => {
          val cell = c.owner.asInstanceOf[cmplx.CellType]

          // for {
          //   expr <- cell.expression
          // } {
          //   OrchardEditor.consoleMessage("Expression: " ++ expr.toString)
          //   OrchardEditor.consoleMessage("Normalized expression: " ++ expr.normalize.toString)
          // }
        }

        case _ => super.onEventEmitted(ev)
      }
    }

    initialize

  }

  //============================================================================================
  // PANEL IMPLEMENTATION
  //

  class FrameworkPanel(val complex : Framework[Option[Expression]], val baseIndex : Int)
      extends ZoomPanel[Option[Expression]] { thisPanel =>

    type ComplexType = Framework[Option[Expression]]

    type CellType = FrameworkPanelCell
    type EdgeType = FrameworkPanelEdge

    override def newCell(owner : complex.CellType) : CellType = {
      val cell = new FrameworkPanelCell(owner)
      owner.registerPanelCell(thisPanel)(cell)
      reactTo(cell)
      cell
    }
    
    override def newEdge(owner : complex.CellType) : EdgeType = {
      val edge = new FrameworkPanelEdge(owner)
      owner.registerPanelEdge(thisPanel)(edge)
      reactTo(edge)
      edge
    }

    class FrameworkPanelCell(val owner : complex.CellType) extends JavaFXCell { thisCell : CellType =>

      def renderLabel : jfxs.Node = {
        val labelNode =
          item match {
            case None => new Region { prefWidth = 10 ; prefHeight = 10 }
            case Some(expr) => new Text(expr.name)
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

    class FrameworkPanelEdge(val owner : complex.CellType) extends JavaFXEdge { thisEdge : EdgeType => }

    //============================================================================================
    // INITIALIZATION
    //

    var baseCell : FrameworkPanelCell = newCell(complex.baseCells(baseIndex))

    refreshPanelData
    initializeChildren

  }
  
}
