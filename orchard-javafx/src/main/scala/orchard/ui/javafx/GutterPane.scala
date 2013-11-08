/**
  * GutterPane.scala - A Pane with a retractable (bottom) gutter
  * 
  * @author Eric Finster
  * @version 0.1
  */

package orchard.ui.javafx

import javafx.scene.Node

import javafx.scene.layout.Pane
import javafx.scene.layout.Region
import javafx.scene.layout.StackPane
import javafx.scene.shape.Rectangle
import javafx.scene.effect.ColorAdjust

import javafx.animation.Timeline
import javafx.animation.KeyValue
import javafx.animation.KeyFrame

import javafx.event.Event
import javafx.event.ActionEvent
import javafx.event.EventHandler

import javafx.util.Duration

import javafx.beans.property.ObjectProperty
import javafx.beans.property.SimpleObjectProperty
import javafx.beans.property.SimpleBooleanProperty
import javafx.beans.property.ReadOnlyBooleanProperty

import javafx.beans.value.WritableValue

class GutterPane extends Region {

  //============================================================================================
  // INITIALIZATION
  //

  getStyleClass.add("gutter-pane")

  private val mainPane = new StackPane
  private val gutterPane = new StackPane

  gutterPane.getStyleClass.add("gutter")
  mainPane.setStyle("-fx-background-color: gainsboro;")
  gutterPane.setStyle("-fx-background-color: white;")

  private val containerPane = new Pane
  containerPane.setStyle("-fx-border-color: purple; -fx-border-width: 1pt; -fx-border-style: solid;")
  containerPane.getChildren add mainPane
  containerPane.getChildren add gutterPane
  getChildren add containerPane

  private val clipRectangle = new Rectangle
  private val colorAdjust = new ColorAdjust

  mainPane.setEffect(colorAdjust)
  containerPane.setClip(clipRectangle)

  //============================================================================================
  // LAYOUT AND RESIZING
  //

  override def layoutChildren = {
    val insets = getInsets
    val left = insets.getLeft
    val right = insets.getRight
    val top = insets.getTop
    val bottom = insets.getBottom

    val gutterHeight = gutterPane.prefHeight(-1)


    mainPane.setPrefSize(getWidth - left - right, getHeight - top - bottom)
    gutterPane.setPrefSize(getWidth - left - right, gutterHeight)

    containerPane.resizeRelocate(left, top, getWidth - left - right, getHeight - top - bottom)

    // Size the clip rectangle
    clipRectangle.setWidth(getWidth - left - right)
    clipRectangle.setHeight(getHeight - top - bottom)

    // Will this f-up the animation?
    if (isGutterExtended) {
      gutterPane.setTranslateY(getHeight - top - bottom - gutterHeight)
    } else {
      gutterPane.setTranslateY(getHeight - top - bottom)
    }
  }

  override def computePrefWidth(height : Double) : Double = 
    getInsets.getLeft + mainPane.prefWidth(height) + getInsets.getRight

  override def computePrefHeight(width : Double) : Double = 
    getInsets.getTop + mainPane.prefHeight(width) + getInsets.getBottom

  //============================================================================================
  // HIDING AND SHOWING
  //

  def hide = {
    val hideAnimation = new Timeline

    val kv1 = new KeyValue(gutterPane.translateYProperty.asInstanceOf[WritableValue[Double]], 
                           getHeight - getInsets.getTop - getInsets.getBottom)
    val kv2 = new KeyValue(colorAdjust.brightnessProperty.asInstanceOf[WritableValue[Double]], 0 : Double)
    val kf1 = new KeyFrame(Duration.millis(200), kv1, kv2)

    hideAnimation.getKeyFrames.add(kf1)

    hideAnimation.setOnFinished(
      new EventHandler[ActionEvent] {
        def handle(ev : ActionEvent) {
          myIsGutterExtendedProperty.setValue(false)
          mainPane.setDisable(false)
          gutterPane.setDisable(true)
          if (onGutterHide != null) { onGutterHide() }
        }
      })

    hideAnimation.play
  }

  def show = {
    val showAnimation = new Timeline

    val kv1 = new KeyValue(gutterPane.translateYProperty.asInstanceOf[WritableValue[Double]], 
                           getHeight - getInsets.getTop - getInsets.getBottom - gutterPane.getHeight)
    val kv2 = new KeyValue(colorAdjust.brightnessProperty.asInstanceOf[WritableValue[Double]], -0.5 : Double)
    val kf1 = new KeyFrame(Duration.millis(200), kv1, kv2)

    showAnimation.getKeyFrames.add(kf1)

    showAnimation.setOnFinished(
      new EventHandler[ActionEvent] {
        def handle(ev : ActionEvent) {
          myIsGutterExtendedProperty.setValue(true)
          mainPane.setDisable(true)
          gutterPane.setDisable(false)
          if (onGutterShow != null) { onGutterShow() }
        }
      })

    showAnimation.play
  }

  //============================================================================================
  // PROPERTIES
  //

  var onGutterShow : () => Unit = null
  var onGutterHide : () => Unit = null

  val mainProperty : ObjectProperty[Node] = new SimpleObjectProperty[Node](this, "main", null)
  val gutterProperty : ObjectProperty[Node] = new SimpleObjectProperty[Node](this, "gutter", null)

  private val myIsGutterExtendedProperty = 
    new SimpleBooleanProperty(this, "isGutterExtended", false)

  val isGutterExtendedProperty : ReadOnlyBooleanProperty = myIsGutterExtendedProperty

  def main : Node = mainProperty.getValue
  def main_=(n : Node) = { 
      mainProperty.setValue(n)
      mainPane.getChildren.setAll(n) 
    }

  def gutter : Node = gutterProperty.getValue
  def gutter_=(n : Node) = { 
      gutterProperty.setValue(n)
      gutterPane.getChildren.setAll(n) 
    }

  def isGutterExtended : Boolean = isGutterExtendedProperty.getValue
}
