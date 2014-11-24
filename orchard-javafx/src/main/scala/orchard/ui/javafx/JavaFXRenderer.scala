/**
  * JavaFXRenderer.scala - JavaFX Renderer Implementation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scala.collection.JavaConversions._

import javafx.scene._
import javafx.scene.text._
import javafx.scene.shape._
import javafx.scene.paint._
import javafx.scene.layout._

import javafx.event.Event
import javafx.event.EventHandler
import javafx.scene.input.MouseEvent

import orchard.core.tree._
import Complex._

class JavaFXRenderer(editor : JavaFXEditor) extends Renderer[Double, Int] {

  def arcRadius : Double = 4.0
  def externalPadding : Double = 5.0
  def internalPadding : Double = 5.0
  def halfLeafWidth : Double = 5.0
  def halfStrokeWidth : Double = 1.0

  type CanvasType = JavaFXCanvas

  type BoxType = JavaFXLabeledBox
  type ExternalBoxType = JavaFXExternalBox
  type InternalBoxType = JavaFXInternalBox

  type EdgeType = JavaFXEdge

  def createNestingCanvas : NestingCanvas = {
    val canvas = new JavaFXCanvas
    editor.renderingSurface.getChildren.add(canvas)
    canvas
  }

  override def renderComplex[N <: Nat](cmplx : Complex[N, Int]) : Option[Complex[N, JavaFXLabeledBox]] = 
    for {
      result <- super.renderComplex(cmplx)
      cm <- comultiply(result)
      zc <- zipComplex(result, cm)
      _ = mapComplex(zc)({
        case (lb, fc) => lb.faceComplex = Some(fc)
      })
    } yield result

  class JavaFXCanvas extends Region with NestingCanvas {

    val group = new Group
    getChildren add group


    import scala.collection.mutable.ListBuffer

    val boxBuffer : ListBuffer[JavaFXLabeledBox] = new ListBuffer
    val edgeBuffer : ListBuffer[JavaFXEdge] = new ListBuffer

    def createExternalBox(a : Int) : JavaFXExternalBox = {
      val box = new JavaFXExternalBox(a)
      box +=: boxBuffer
      box
    }

    def createInternalBox(a : Int, layout : BoxLayout) : JavaFXInternalBox = {
      val box = new JavaFXInternalBox(a, layout)
      box +=: boxBuffer
      box
    }

    def createEdge(a : Int) : JavaFXEdge = {
      val edge = new JavaFXEdge(a)
      edge +=: edgeBuffer
      edge
    }

    override def finalizeRenderPass = {

      val children = group.getChildren

      for { box <- boxBuffer } { 
        box.relocate(box.x, box.y)
        children.add(box) 
      }

      for { edge <- edgeBuffer } { 
        edge.renderPath 
        children.add(edge) 
      }

    }

    override def layoutChildren = {

      group.autosize
      group.relocate(getInsets.getLeft, getInsets.getTop)

    }

  }

  abstract class JavaFXLabeledBox(val a : Int) extends Region with LabeledBox {

    val owner : Int = a
    val label = new Text(a.toString)

    val pane = new Pane
    pane.getChildren.add(label)
    this.getChildren.add(pane)

    setPrefWidth(width)
    setPrefHeight(height)

    //  We should set these by hand based on rendering info ...
    getStyleClass add "orch-cell"

    val cellStyleIndex = getStyleClass.length
    val cellSelectedStyleIndex = cellStyleIndex + 1
    val cellHoveredStyleIndex = cellStyleIndex + 2

    getStyleClass add "orch-cell-unknown"
    getStyleClass add "orch-cell-unknown-selected"
    getStyleClass add "orch-cell-unknown-hovered"

    def setCellStyle(style : String) = getStyleClass(cellStyleIndex) = style
    def setCellSelectedStyle(style : String) = getStyleClass(cellSelectedStyleIndex) = style
    def setCellHoveredStyle(style : String) = getStyleClass(cellHoveredStyleIndex) = style

    def removeCellStyle = setCellStyle("orch-cell-unknown")
    def removeCellSelectedStyle = setCellSelectedStyle("orch-cell-unknown-selected")
    def removeCellHoveredStyle = setCellHoveredStyle("orch-cell-unknown-hovered")

    def assignStyle = setCellStyle("orch-cell-" ++ getStyleString)

    def doHover = setCellHoveredStyle("orch-cell-" ++ getStyleString ++ "-hovered")
    def doSelect = setCellSelectedStyle("orch-cell-" ++ getStyleString ++ "-selected")

    def doUnhover = removeCellHoveredStyle
    def doDeselect = removeCellSelectedStyle

    def getStyleString = "empty"

    //============================================================================================
    // EVENTS
    //

    val mouseHandler =
      new EventHandler[MouseEvent] {
        def handle(ev : MouseEvent) {
          ev.getEventType match {
            case MouseEvent.MOUSE_ENTERED => for { fc <- faceComplex } { mapComplex(fc.value)(_.doHover) }
            case MouseEvent.MOUSE_EXITED => for { fc <- faceComplex } { mapComplex(fc.value)(_.doUnhover) }
            case MouseEvent.MOUSE_CLICKED => editor.consoleWrite("Clicked box: " ++ a.toString)
            case _ => ()
          }
          ev.consume
        }
      }

    pane.addEventHandler(MouseEvent.ANY, mouseHandler)

    override def layoutChildren = {

      pane.resizeRelocate(0, 0, getWidth, getHeight)
      
      label.relocate(
        getWidth - strokeWidth - internalPadding - labelWidth ,
        getHeight - strokeWidth - internalPadding - labelHeight
      )

    }

    //============================================================================================
    // BOX DATA
    //

    var rootX : Double = 0.0
    var rootY : Double = 0.0

    def halfLabelWidth : Double = label.getLayoutBounds.getWidth / 2
    def halfLabelHeight : Double = label.getLayoutBounds.getHeight / 2

    var faceComplex : Option[Sigma[Complex, JavaFXLabeledBox]] = None

  }

  class JavaFXExternalBox(a : Int) extends JavaFXLabeledBox(a) with ExternalBox

  class JavaFXInternalBox(a : Int, l : BoxLayout) extends JavaFXLabeledBox(a) with InternalBox {

    def interior : BoxLayout = l

  }

  class JavaFXEdge(a : Int) extends Path with Edge {

    val owner : Int = a

    getStyleClass add "orch-edge"

    setStroke(Color.BLACK)
    setStrokeWidth(strokeWidth)

    def renderPath = {
      val startMove = new MoveTo(startX, startY)

      if (startX == endX) {
        val vertLine = new VLineTo(endY)
        getElements.setAll(List(startMove, vertLine))
      } else {
        val vertLine = new VLineTo(endY - arcRadius)

        val arcTo = new ArcTo
        arcTo.setX(if (startX > endX) (startX - arcRadius) else (startX + arcRadius))
        arcTo.setY(endY)
        arcTo.setRadiusX(arcRadius)
        arcTo.setRadiusY(arcRadius)
        arcTo.setSweepFlag(startX > endX)

        val horizLine = new HLineTo(endX)

        getElements.setAll(List(startMove, vertLine, arcTo, horizLine))
      }
    }

  }

}
