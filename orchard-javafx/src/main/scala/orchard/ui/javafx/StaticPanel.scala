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

abstract class StaticPanel[A] extends JavaFXPanel[A] { thisPanel =>

  getStyleClass add "orch-static-panel"

  protected val myChildGroup = new Group

  childGroup.setManaged(false)
  getChildren.add(childGroup)

  def childGroup = myChildGroup

  override def layoutChildren : Unit = {
    super.layoutChildren
    childGroup.relocate(getInsets.getLeft, getInsets.getTop)
  }

  override def computePrefWidth(height : Double) : Double = getInsets.getLeft + childGroup.prefWidth(height) + getInsets.getRight
  override def computePrefHeight(width : Double) : Double = getInsets.getTop + childGroup.prefHeight(width) + getInsets.getBottom

  override def computeMinWidth(height : Double) : Double = computePrefWidth(height)
  override def computeMinHeight(width : Double) : Double = computePrefHeight(width)

}
