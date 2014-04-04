/**
  * ZoomPanel.scala - A panel which zooms its contents to size
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import javafx.scene.Group
import javafx.scene.layout.Region
import javafx.scene.transform.Scale

abstract class ZoomPanel[A] extends JavaFXPanel[A] {

  getStyleClass add "orch-zoom-panel"

  protected val myChildGroup = new Group
  protected val childScaleTransform = new Scale(1.0, 1.0, 0.0, 0.0)

  childGroup.setManaged(false)
  childGroup.getTransforms.add(childScaleTransform)
  getChildren.add(childGroup)

  def childGroup = myChildGroup

  override def resize(width : Double, height : Double) = {
    val bounds = childGroup.getLayoutBounds

    childScaleTransform.setPivotX(bounds.getMinX)
    childScaleTransform.setPivotY(bounds.getMinY)

    val xfactor = (width - getInsets.getLeft - getInsets.getRight) / bounds.getWidth
    val yfactor = (height - getInsets.getTop - getInsets.getBottom) / bounds.getHeight 

    if (xfactor < yfactor) {
      if (xfactor <= 1.0) {
        childScaleTransform.setX(xfactor)
        childScaleTransform.setY(xfactor)
      } else {
        childScaleTransform.setX(1.0)
        childScaleTransform.setY(1.0)
      }
    } else {
      if (yfactor <= 1.0) {
        childScaleTransform.setX(yfactor)
        childScaleTransform.setY(yfactor)
      } else {
        childScaleTransform.setX(1.0)
        childScaleTransform.setY(1.0)
      }
    }

    super.resize(width, height)
  }

  override def layoutChildren : Unit = {
    val bounds = childGroup.getBoundsInParent

    val emptyX = getWidth() - getInsets.getLeft - getInsets.getRight - bounds.getWidth
    val emptyY = getHeight() - getInsets.getTop - getInsets.getBottom - bounds.getHeight

    childGroup.relocate(getInsets.getLeft + (emptyX / 2), getInsets.getTop + (emptyY / 2))
  }

  override def computePrefWidth(height : Double) : Double = {
    getInsets.getLeft + childGroup.prefWidth(height) + getInsets.getRight
  }

  override def computePrefHeight(width : Double) : Double = {
    getInsets.getTop + childGroup.prefHeight(width) + getInsets.getBottom
  }

}
