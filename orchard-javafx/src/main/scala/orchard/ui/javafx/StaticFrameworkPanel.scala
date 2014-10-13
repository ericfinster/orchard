/**
  * StaticFrameworkPanel.scala - A static implementation of a framework panel
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.scene.text.Text
import scalafx.scene.layout.Region

import orchard.core.cell._
import orchard.core.complex._
import orchard.core.expression._

import javafx.{scene => jfxs}

class StaticFrameworkPanel(val complex : Framework[Option[Expression]], val baseIndex : Int) extends StaticPanel[Option[Expression]] { thisPanel =>

  type ComplexType = Framework[Option[Expression]]

  type CellType = StaticFrameworkCell
  type EdgeType = StaticFrameworkEdge

  class StaticFrameworkCell(val owner : complex.CellType) extends JavaFXCell {

    def renderLabel : jfxs.Node = {
      val labelNode =
        item match {
          case None => new Region { prefWidth = 10 ; prefHeight = 10 }
          case Some(expr) => new Text(expr.id)
        }

      labelNode.layoutBounds onChange { thisPanel.refresh }
      pane.getChildren.setAll(labelNode)
      labelNode
    }

    def getStyleString =
      item match {
        case None => if (owner.isExposedNook) "exposed" else "empty" 
        case Some(expr) => expr.styleString
      }

    renderCell

    override def onEventEmitted(ev : CellEvent) = {
      ev match {
        case complex.ChangeEvents.ItemChangedEvent(oldItem) => { renderCell ;  super.onEventEmitted(ev) }
        case CellEntered(cell) => owner.emit(RequestCellHovered) ; owner.emit(RequestEdgeHovered)
        case CellExited(cell) => owner.emit(RequestCellUnhovered) ; owner.emit(RequestEdgeUnhovered)
        case _ => super.onEventEmitted(ev)
      }
    }

  }


  class StaticFrameworkEdge(val owner : complex.CellType) extends JavaFXEdge

  def newCell(owner : complex.CellType) = { 
    val frameworkCell = new StaticFrameworkCell(owner)
    owner.registerPanelCell(thisPanel)(frameworkCell)
    frameworkCell
  }

  def newEdge(owner : complex.CellType) = {
    val frameworkEdge = new StaticFrameworkEdge(owner)
    owner.registerPanelEdge(thisPanel)(frameworkEdge)
    frameworkEdge
  }

  //============================================================================================
  // INITIALIZATION
  //

  var baseCell : StaticFrameworkCell = newCell(complex.baseCells(baseIndex))

  refreshPanelData
  initializeChildren

}
