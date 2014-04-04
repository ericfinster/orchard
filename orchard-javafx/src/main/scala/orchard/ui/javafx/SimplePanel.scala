/**
  * SimplePanel.scala - A Simple Panel Implementation
  * 
  * @author Eric Finster
  * @version 0.1
  */

package orchard.ui.javafx

import javafx.scene.Node
import javafx.scene.text.Text

import orchard.core._

class SimplePanel[A](val complex : SimpleMutableComplex[A], baseIndex : Int) extends ZoomPanel[A] { thisPanel =>

  type CellType = SimpleCell
  type EdgeType = SimpleEdge

  type ComplexType = SimpleMutableComplex[A]

  class SimpleCell(val owner : complex.SimpleMutableCell) extends JavaFXCell {

    def renderLabel : Node = { 
      val lbl = new Text(item.toString)
      pane.getChildren.setAll(lbl)
      lbl
    }

    def getStyleString : String = "orch-simple-cell"

    //============================================================================================
    // CELL EVENTS
    //

    override def onEventEmitted(ev : CellEvent) = {
      ev match {
        case CellClicked(cell) => owner.dumpInfo
        case _ => super.onEventEmitted(ev)
      }
    }

    override def toString = "Cell(" ++ item.toString ++ ")@" ++ hashCode.toString
  }

  class SimpleEdge(val owner : complex.SimpleMutableCell) extends JavaFXEdge

  def newCell(owner : complex.SimpleMutableCell) : SimpleCell = {
    val simpleCell = new SimpleCell(owner)
    owner.registerPanelCell(thisPanel)(simpleCell)
    simpleCell
  }

  def newEdge(owner : complex.SimpleMutableCell) : SimpleEdge = {
    val simpleEdge = new SimpleEdge(owner)
    owner.registerPanelEdge(thisPanel)(simpleEdge)
    simpleEdge
  }

  //============================================================================================
  // UI INITIALIZATION
  //

  var baseCell : SimpleCell = newCell(complex.baseCells(baseIndex))

  refreshPanelData
  initializeChildren

}
