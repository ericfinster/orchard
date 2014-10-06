/**
  * JavaFXSVGPanel.scala - Special rendering for SVG in JavaFX
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx.svg

import scalafx.scene.web.WebEngine

import javafx.concurrent.Task
import javafx.concurrent.Worker.State
import javafx.beans.value.ChangeListener
import javafx.beans.value.ObservableValue

import orchard.core.ui._
import orchard.core.svg._
import orchard.core.cell._
import orchard.core.complex._
import orchard.core.expression._

import netscape.javascript.JSObject

import xml._

abstract class JavaFXSVGPanel[A](engine : WebEngine) extends SVGPanel[A] { thisPanel =>

  type CellType <: JavaFXSVGCell
  type EdgeType <: JavaFXSVGEdge

  def toSVG : NodeSeq = {
    var pathNodes : NodeSeq = Seq.empty

    for { tgt <- baseCell.target } {
      tgt foreachEdge (edge => pathNodes ++= edge.toSVG)
    }

    <g>{baseCell.toSVG ++ pathNodes}</g>
  }

  // Okay, the problem is that for SVG this now becomes asynchronous.
  def setLabelSizes = {
    baseCell foreachCell (cell => cell.setLabelSize)
  }

  var onRenderPassFinished : Unit => Unit = (_ => ())

  def fullRenderPass = {
    engine.getLoadWorker.stateProperty.addListener(
      new ChangeListener[State]{
        def changed(ov : ObservableValue[_ <: State], oldState : State, newState : State) {
          newState match {
            case State.SUCCEEDED => {
              println("Finished rendering the labels")
              setLabelSizes
              thisPanel.render
              onRenderPassFinished()
            }
            case _ => println("Unrecognized state change.")
          }
        }
      })

    engine.loadContent((<svg>{labelProofSheet}</svg>).toString)
  }

  abstract class JavaFXSVGCell extends SVGCell { thisCell : CellType =>

    def svgId : String

    def setLabelSize = {
      // Should use a separate method which looks up the svgId, since we may want
      // to override it in subclasses
      val scriptStr = "document.getElementById(\"cell-label-" ++ svgId ++ "\").getBBox();"

      // This should return the bounding box of the element in the proof sheet ...
      val bbox = engine.executeScript(scriptStr).asInstanceOf[JSObject]

      labelWidth = bbox.getMember("width").asInstanceOf[Int]
      labelHeight = bbox.getMember("height").asInstanceOf[Int]

      println("Setting label size to: " ++ labelWidth.toString ++ ", " ++ labelHeight.toString ++ ")")
    }

  }

  abstract class JavaFXSVGEdge extends SVGEdge { thisEdge : EdgeType =>

    var pathString : String = ""

    def renderPath = {
      pathString = "M " ++ incomingX.toString ++ " " ++ incomingY.toString ++ " "

      if (isVertical) {
        pathString ++= "V " ++ outgoingY.toString
      } else {
        pathString ++= "V " ++ (outgoingY - arcRadius).toString ++ " "
        pathString ++= "A " ++ arcRadius.toString ++ " " ++ arcRadius.toString ++ " 0 0 " ++ (if (incomingX > outgoingX) "1 " else "0 ") ++ 
          (if (incomingX > outgoingX) (incomingX - arcRadius) else (incomingX + arcRadius)).toString ++ " " ++ outgoingY.toString ++ " "
        pathString ++= "H " ++ outgoingX.toString
      }
    }

  }

}
