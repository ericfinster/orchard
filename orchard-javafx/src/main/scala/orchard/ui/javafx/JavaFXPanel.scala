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

import javafx.event.Event
import javafx.event.EventHandler
import javafx.scene.input.MouseEvent

abstract class JavaFXPanel[A] extends Region with RenderingPanel[A] { thisPanel =>

  override type CellType <: JavaFXCell
  override type EdgeType <: JavaFXEdge

  //============================================================================================
  // UI INITIALIZATION
  //

  def childGroup : Group
  def baseCell : CellType

  def initializeChildren = {
    childGroup.getChildren.clear
    baseCell foreachCell (cell => cell.collectChildren)
    childGroup.getChildren.add(baseCell)

    for { tgt <- baseCell.target} {
      tgt foreachEdge (edge => addEdge(edge))
    }
  }

  def setLabelSizes = {
    baseCell foreachCell (cell => cell.setLabelSize)
  }

  def addEdge(edge : EdgeType) = childGroup.getChildren.add(edge)
  def removeEdge(edge : EdgeType) = childGroup.getChildren.remove(edge)

  //============================================================================================
  // RENDERING
  //

  override def render = {
    setLabelSizes
    super.render
    baseCell.relocate(baseCell.x, baseCell.y)
    baseCell foreachCell (cell => cell.setDimensions)
    childGroup.layout
  }

  override def refresh = {
    // It would be better to understand exactly how the child relationships
    // have changed and modify accordingly, but for now, brute force ...
    clearRenderState
    initializeChildren
    render
    requestLayout
  }

  //============================================================================================
  // EVENTS
  //

  addEventHandler(MouseEvent.MOUSE_CLICKED, new EventHandler[MouseEvent] {
    def handle(ev : MouseEvent) = {
      emit(PanelClicked)
    }
  })

  //============================================================================================
  // CELL AND EDGE IMPLEMENTATION
  //

  abstract class JavaFXCell extends Region with VisualCell { thisCell : CellType =>

    //============================================================================================
    // UI INITIALIZATION
    //

    getStyleClass add "orch-cell"

    val cellStyleIndex = getStyleClass.length
    val cellSelectedStyleIndex = cellStyleIndex + 1
    val cellHoveredStyleIndex = cellStyleIndex + 2

    getStyleClass add "orch-cell-unknown"
    getStyleClass add "orch-cell-unknown-selected"
    getStyleClass add "orch-cell-unknown-hovered"

    val pane = new Pane

    var label : Node = new Region

    def renderLabel : Node
    def getStyleString : String

    def renderCell = {
      assignStyle
      label = renderLabel
    }

    def collectChildren = {
      val myChildren = getChildren

      myChildren.clear
      myChildren.add(pane)
      
      for { tree <- canopy } {
        tree foreachCell (cell => myChildren.add(cell))
      }
    }

    def setLabelSize = {
      val w = label.getLayoutBounds.getWidth
      val h = label.getLayoutBounds.getHeight

      // Set a minimum here???
      labelWidth = w
      labelHeight = h + (if (isExternal) 0.0 else internalPadding)
    }

    def setCellStyle(style : String) = getStyleClass(cellStyleIndex) = style
    def setCellSelectedStyle(style : String) = getStyleClass(cellSelectedStyleIndex) = style
    def setCellHoveredStyle(style : String) = getStyleClass(cellHoveredStyleIndex) = style

    def removeCellStyle = setCellStyle("orch-cell-unknown")
    def removeCellSelectedStyle = setCellSelectedStyle("orch-cell-unknown-selected")
    def removeCellHoveredStyle = setCellHoveredStyle("orch-cell-unknown-hovered")

    def assignStyle = setCellStyle("orch-cell-" ++ getStyleString)

    override def doHover = setCellHoveredStyle("orch-cell-" ++ getStyleString ++ "-hovered")
    override def doSelect = setCellSelectedStyle("orch-cell-" ++ getStyleString ++ "-selected")

    override def doUnhover = removeCellHoveredStyle
    override def doDeselect = removeCellSelectedStyle

    //============================================================================================
    // EVENTS
    //

    val mouseHandler =
      new EventHandler[MouseEvent] {
        def handle(ev : MouseEvent) {
          ev.getEventType match {
            case MouseEvent.MOUSE_ENTERED => owner.emit(CellEntered(thisCell))
            case MouseEvent.MOUSE_EXITED => owner.emit(CellExited(thisCell))
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
          ev.consume
        }
      }

    pane.addEventHandler(MouseEvent.ANY, mouseHandler)

    //============================================================================================
    // LAYOUT AND RENDERING
    //

    override def layoutChildren = {
      for { tree <- canopy } {
        tree foreachCell
        (cell => {
           cell.autosize
           cell.relocate(cell.x - x, cell.y - y)
         })
      }

      pane.resizeRelocate(0, 0, getWidth, getHeight)

      if (label != null)
          label.relocate(width - labelWidth - internalPadding - strokeWidth,
                         height - label.getLayoutBounds.getHeight - internalPadding - strokeWidth)
    }

    def setDimensions = {
      setPrefWidth(width)
      setPrefHeight(height)
    }

    override def toString = "Cell(" ++ item.toString ++ ")@" ++ hashCode.toString
  }

  abstract class JavaFXEdge extends Path with VisualEdge { thisEdge : EdgeType =>

    //============================================================================================
    // UI INITIALIZATION
    //

    getStyleClass().add("orch-edge")

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

    override def doHover : Unit = setStroke(Color.TOMATO)
    override def doSelect : Unit = setStroke(Color.TOMATO)
    override def doUnhover : Unit = setStroke(Color.BLACK)
    override def doDeselect : Unit = setStroke(Color.BLACK)

    override def toString = "Edge(" ++ item.toString ++ ")"
  }
}
