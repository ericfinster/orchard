/**
  * Spinner.scala - A Spinner
  * 
  * @author Eric Finster
  * @version 0.1
  */

package orchard.ui.javafx.controls

import scala.collection.JavaConversions._

import javafx.collections.FXCollections
import javafx.collections.ObservableList
import javafx.collections.ListChangeListener

import javafx.scene.Node
import javafx.scene.layout.Region
import javafx.scene.layout.HBox

import javafx.scene.shape.Rectangle

import javafx.beans.property.IntegerProperty
import javafx.beans.property.SimpleIntegerProperty
import javafx.beans.property.SimpleObjectProperty
import javafx.beans.property.ReadOnlyObjectProperty

import javafx.beans.value.WritableValue

import javafx.animation.Timeline
import javafx.animation.KeyValue
import javafx.animation.KeyFrame

import javafx.geometry.Pos
import javafx.util.Duration

class Spinner extends Region {

  //============================================================================================
  // INITIALIZATION
  //

  getStyleClass.add("spinner")

  protected val clipRectangle = new Rectangle

  protected val hbox = new HBox
  hbox.setSpacing(10)
  hbox.setAlignment(Pos.CENTER)
  hbox.setClip(clipRectangle)

  getChildren.add(hbox)

  //============================================================================================
  // PROPERTIES
  //

  def spacing = hbox.getSpacing
  def spacing_=(d : Double) = hbox.setSpacing(d)

  val lengthProperty : IntegerProperty = new SimpleIntegerProperty(this, "length", 5)

  def length = lengthProperty.getValue
  def length_=(n : Int) = lengthProperty.setValue(n)

  val indexProperty : IntegerProperty = new SimpleIntegerProperty(this, "index", 0)

  def index = indexProperty.getValue
  def index_=(i : Int) = seek(i)

  def items = itemsProperty.getValue

  val itemsProperty : ReadOnlyObjectProperty[ObservableList[Node]] = 
    new SimpleObjectProperty(this, "items", hbox.getChildren)

  //============================================================================================
  // LAYOUT
  //

  // The "toInt" seems to give the best pixel alignment

  def visibleWidth = getWidth - getInsets.getLeft - getInsets.getRight
  def regionWidth = ((visibleWidth - (hbox.getSpacing * (length - 1))) / length).toInt

  override def layoutChildren = {
    val insets = getInsets
    val left = insets.getLeft
    val right = insets.getRight
    val top = insets.getTop
    val bottom = insets.getBottom

    // This is ugly.  I think you should probably keep a separate list which is syned with hbox's children
    items foreach (item => item.asInstanceOf[Region].setPrefSize(regionWidth, getHeight - top - bottom))

    hbox.autosize
    hbox.relocate(left, top)

    clipRectangle.setWidth(getWidth - left - right)
    clipRectangle.setHeight(hbox.getHeight)
  }

  override def computePrefWidth(height : Double) : Double = 
    getInsets.getLeft + hbox.prefWidth(height) + getInsets.getRight

  override def computePrefHeight(width : Double) : Double = 
    getInsets.getTop + hbox.prefHeight(width) + getInsets.getBottom

  //============================================================================================
  // SEMANTICS
  //

  def next = {
    seek(index + 1)
  }

  def prev = {
    seek(index - 1)
  }

  def seek(i : Int) = {
    if (i < 0 || i > (items.length - length)) () else {
      val targetTranslateX = (regionWidth + hbox.getSpacing) * i

      val seekAnimation = new Timeline
      val kv1 = new KeyValue(hbox.translateXProperty.asInstanceOf[WritableValue[Double]],
                             -targetTranslateX)
      val kv2 = new KeyValue(clipRectangle.translateXProperty.asInstanceOf[WritableValue[Double]],
                             targetTranslateX)
      val kf1 = new KeyFrame(Duration.millis(250), kv1, kv2)
      seekAnimation.getKeyFrames.add(kf1)
      seekAnimation.play

      indexProperty.setValue(i)
    }
  }

  def fastForward = {
    seek(items.length - length)
  }

  def rewind = {
    seek(0)
  }
}
