/**
  * JavaFXPanel.scala - A Panel Implementation in JavaFX
  * 
  * @author Eric Finster
  * @version 0.1
  */

package orchard.ui.javafx

import scala.collection.JavaConversions._

import orchard.core._

import javafx.scene.Node
import javafx.scene.Group

import javafx.scene.text.Text
import javafx.scene.layout.Pane
import javafx.scene.layout.Region

import javafx.scene.shape.Path
import javafx.scene.shape.VLineTo
import javafx.scene.shape.HLineTo
import javafx.scene.shape.MoveTo
import javafx.scene.shape.ArcTo

import javafx.scene.paint.Color
import javafx.scene.transform.Scale

import javafx.event.Event
import javafx.event.EventHandler
import javafx.scene.input.MouseEvent

trait JavaFXRenderer[A] {
  def render(a : A) : Node
}

abstract class JavaFXPanel[A] extends Region with RenderingPanel[A] {

  override type CellType <: JavaFXCell
  override type EdgeType <: JavaFXEdge

  //============================================================================================
  // UI INITIALIZATION
  //

  getStyleClass().add("javafx-panel")

  protected val childGroup = new Group
  protected val childScaleTransform = new Scale(1.0, 1.0, 0.0, 0.0)

  childGroup.setManaged(false)
  childGroup.getTransforms.add(childScaleTransform)
  getChildren.add(childGroup)

  def baseCell : CellType

  def initializeChildren = {
    childGroup.getChildren.clear
    baseCell foreachCell (cell => cell.collectChildren)
    childGroup.getChildren.add(baseCell)

    for { tgt <- baseCell.target} {
      tgt foreachEdge (edge => addEdge(edge))
    }
  }

  def addEdge(edge : EdgeType) = childGroup.getChildren.add(edge)
  def removeEdge(edge : EdgeType) = childGroup.getChildren.remove(edge)

  //============================================================================================
  // RENDERING
  //

  override def render = {
    super.render

    baseCell.relocate(baseCell.x, baseCell.y)
    baseCell foreachCell (cell => cell.setDimensions)
    childGroup.layout
  }

  def refresh = {
    // It would be better to understand exactly how the child relationships
    // have changed and modify accordingly, but for now, brute force ...
    clearRenderState
    initializeChildren
    render
    requestLayout
  }

  //============================================================================================
  // LAYOUT AND RESIZING
  //

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

  //============================================================================================
  // CELL AND EDGE IMPLEMENTATION
  //

  abstract class JavaFXCell(val owner : complex.CellType) extends Region with VisualCell { thisCell : CellType =>

    //============================================================================================
    // UI INITIALIZATION
    //

    getStyleClass().add("javafx-cell")

    val pane = new Pane

    var label : Node = renderLabel

    def renderLabel : Node

    def collectChildren = {
      val myChildren = getChildren

      myChildren.clear
      myChildren.add(pane)
      
      for { tree <- shell } {
        tree foreachCell (cell => myChildren.add(cell))
      }
    }

    def setLabelSize = {
      val w = label.getLayoutBounds.getWidth
      val h = label.getLayoutBounds.getHeight

      // Set a minimum here???
      labelWidth = w
      labelHeight = h
    }

    //============================================================================================
    // EVENTS
    //

    val mouseHandler =
      new EventHandler[MouseEvent] {
        def handle(ev : MouseEvent) {
          ev.getEventType match {
            case MouseEvent.MOUSE_ENTERED =>
              owner.emit(CellEntered(thisCell))
            case MouseEvent.MOUSE_EXITED =>
              owner.emit(CellExited(thisCell))
            case MouseEvent.MOUSE_CLICKED => {
              if (ev.getClickCount > 1) {
                owner.emit(CellDoubleClicked(thisCell))
              } else {
                if (ev.isControlDown) {
                  owner.emit(CellCtrlClicked(thisCell))
                } else {
                  owner.emit(CellClicked(thisCell))
                }
              }
            }
            case _ => ()
          }
        }
      }

    pane.addEventHandler(MouseEvent.ANY, mouseHandler)

    //============================================================================================
    // LAYOUT AND RENDERING
    //

    override def layoutChildren = {
      for { tree <- shell } {
        tree foreachCell
        (cell => {
           cell.autosize
           cell.relocate(cell.x - x, cell.y - y)
         })
      }

      pane.resizeRelocate(0, 0, getWidth, getHeight)

      if (label != null)
        label.relocate(internalWidth + internalPadding + strokeWidth,
                       internalHeight + internalPadding + strokeWidth)
    }

    def setDimensions = {
      setPrefWidth(width)
      setPrefHeight(height)
    }

    override def toString = "Cell(" ++ item.toString ++ ")"
  }

  abstract class JavaFXEdge(val owner : complex.CellType) extends Path with VisualEdge { thisEdge : EdgeType =>

    //============================================================================================
    // UI INITIALIZATION
    //

    getStyleClass().add("javafx-edge")

    setStroke(Color.BLACK)
    setStrokeWidth(strokeWidth)

    //============================================================================================
    // PATH RENDERING
    //

    def renderPath =
    {
      val startMove = new MoveTo(incomingX, incomingY)

      if (isVertical) {
        val vertLine = new VLineTo(outgoingY)
        getElements.setAll(List(startMove, vertLine))
      } else {
        val vertLine = new VLineTo(outgoingY - arcRadius)

        val arcTo = new ArcTo
        arcTo.setX(if (incomingX > outgoingX) (incomingX - arcRadius) else (incomingX + arcRadius))
        arcTo.setY(outgoingY)
        arcTo.setRadiusX(arcRadius)
        arcTo.setRadiusY(arcRadius)
        arcTo.setSweepFlag(incomingX > outgoingX)

        val horizLine = new HLineTo(outgoingX)

        getElements.setAll(List(startMove, vertLine, arcTo, horizLine))
      }
    }

    override def toString = "Edge(" ++ item.toString ++ ")"
  }
}
