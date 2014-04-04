/**
  * ShapeGallery.scala - A Gallery for displaying shape expressions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.scene.text.Text
import scalafx.scene.layout.Region

import orchard.core._

import javafx.{scene => jfxs}

trait ShapeEnvironment { thisEnvironment : JavaFXWorkspace =>

  class ShapePanel(val complex : ShapeFramework, val baseIndex : Int)
      extends ZoomPanel[IndexType]
      with MutablePanel[IndexType] { thisPanel =>
  
    type CellType = ShapePanelCell
    type EdgeType = ShapePanelEdge

    type ComplexType = ShapeFramework

    class ShapePanelCell(val owner : complex.ShapeFrameworkCell) extends JavaFXCell with MutablePanelCell {

      def expression = owner.expression

      def renderLabel : jfxs.Node = {
        val labelNode = 
          expression match {
            case None => new Region { prefWidth = 10 ; prefHeight = 10 }
            case Some(expr) => new Text(thisEnvironment.context.expandIdentifier(expr.ident))
          }

        labelNode.layoutBounds onChange { thisPanel.refresh }
        pane.getChildren.setAll(labelNode)
        labelNode
      }

      def getStyleString = 
        expression match {
          case None => "empty"
          case Some(expr) => expr.styleString
        }

      renderCell

      override def onEventEmitted(ev : CellEvent) = {
        ev match {
          case complex.ChangeEvents.ItemChangedEvent(oldItem) => { renderCell ;  super.onEventEmitted(ev) }
          case CellEntered(cell) => { owner.emitToFaces(RequestCellHovered) ; owner.emit(RequestEdgeHovered) }
          case CellExited(cell) => { owner.emitToFaces(RequestCellUnhovered) ; owner.emit(RequestEdgeUnhovered) }
          case _ => super.onEventEmitted(ev)
        }
      }
    }

    class ShapePanelEdge(val owner : complex.ShapeFrameworkCell) extends JavaFXEdge with MutablePanelEdge
  
    def newCell(owner : complex.ShapeFrameworkCell) : ShapePanelCell = {
      val cell = new ShapePanelCell(owner)
      owner.registerPanelCell(thisPanel)(cell)
      reactTo(cell)
      cell
    }
    
    def newEdge(owner : complex.ShapeFrameworkCell) : ShapePanelEdge = {
      val edge = new ShapePanelEdge(owner)
      owner.registerPanelEdge(thisPanel)(edge)
      reactTo(edge)
      edge
    }
    
    //============================================================================================
    // INITIALIZATION
    //

    var baseCell : ShapePanelCell = newCell(complex.baseCells(baseIndex))

    refreshPanelData
    initializeChildren

  }

  class ShapeGallery(val complex : ShapeFramework) extends SpinnerGallery[IndexType] { thisGallery =>

    // def this(seed : NCell[Seq[Int]]) = this(new ShapeFramework(seed))
    // def this(idx : Int) = this(ShapeFramework(idx))
    // def this(expr : Expression) = this(context.indexOf(expr))

    type PanelType = ShapePanel

    def newPanel(i : Int) : ShapePanel = {
      val panel = new ShapePanel(complex, i)
      reactTo(panel)
      panel
    }

    initialize

  }
}
