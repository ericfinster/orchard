/**
  * StaticPanel.scala - A Static Panel which does no resizing
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import javafx.scene.Node
import javafx.scene.Group
import javafx.scene.text.Text
import javafx.scene.layout.Region
import javafx.scene.transform.Scale

import orchard.core._

class StaticPanel[A](val complex : SimpleMutableComplex[A], baseIndex : Int) extends Region with JavaFXPanel[A] { thisPanel =>

  type CellType = StaticCell
  type EdgeType = StaticEdge

  type ComplexType = SimpleMutableComplex[A]

  getStyleClass add "orch-static-panel"

  protected val myChildGroup = new Group

  childGroup.setManaged(false)
  getChildren.add(childGroup)

  def childGroup = myChildGroup

  override def layoutChildren : Unit = {
    super.layoutChildren
    childGroup.relocate(getInsets.getLeft, getInsets.getTop)
  }

  override def computePrefWidth(height : Double) : Double = {
    getInsets.getLeft + childGroup.prefWidth(height) + getInsets.getRight
  }

  override def computePrefHeight(width : Double) : Double = {
    getInsets.getTop + childGroup.prefHeight(width) + getInsets.getBottom
  }

  //============================================================================================
  // CELL IMPLEMENTATION
  //

  class StaticCell(owner : complex.SimpleMutableCell) extends JavaFXCell(owner) {

    def renderLabel : Node = { 
      val lbl = new Text(item.toString)
      pane.getChildren.setAll(lbl)
      lbl
    }

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

  class StaticEdge(owner : complex.SimpleMutableCell) extends JavaFXEdge(owner)

  def newCell(owner : complex.SimpleMutableCell) : StaticCell = {
    val simpleCell = new StaticCell(owner)
    owner.registerPanelCell(thisPanel)(simpleCell)
    simpleCell
  }

  def newEdge(owner : complex.SimpleMutableCell) : StaticEdge = {
    val simpleEdge = new StaticEdge(owner)
    owner.registerPanelEdge(thisPanel)(simpleEdge)
    simpleEdge
  }

  //============================================================================================
  // UI INITIALIZATION
  //

  var baseCell : StaticCell = newCell(complex.baseCells(baseIndex))

  refreshPanelData
  initializeChildren

}
