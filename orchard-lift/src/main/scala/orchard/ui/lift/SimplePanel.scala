/**
  * SimplePanel.scala - A simple implementation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.lift

import scala.collection.mutable.Map

import xml._
import orchard.core._

case class BBox(val x : Int, val y : Int, val width : Int, val height : Int)

class SimplePanel[A](val complex : SimpleMutableComplex[A], baseIndex : Int) extends LiftPanel[A] { thisPanel =>

  type CellType = SimpleCell
  type EdgeType = SimpleEdge

  type ComplexType = SimpleMutableComplex[A]

  def setLabelSizes = ()

  def labelProofSheet : NodeSeq = {
    var lps : NodeSeq = Seq.empty
    baseCell foreachCell (c => lps ++= c.labelSVG)
    lps
  }

  def setLabelsFromBBox(dict : Map[String, BBox]) = {
    baseCell foreachCell (c => c.setLabelFromBBox(dict))
  }

  def toSVG : NodeSeq = {
    val viewBoxStr = (baseCell.x - 10).toString ++ " " ++ (baseCell.y - 10).toString ++ " " ++ 
      (baseCell.width + 20).toString ++ " " ++ (baseCell.height + 20).toString

    var pathNodes : NodeSeq = Seq.empty

    for { tgt <- baseCell.target } {
      tgt foreachEdge (edge => pathNodes ++= edge.toSVG)
    }

    // labels are done ... now the edges ...
    <svg width="400" height="400" viewBox={viewBoxStr}>
      <g>{baseCell.toSVG ++ pathNodes}</g>
    </svg>
  }

  def refresh = ()

  class SimpleCell(val owner : complex.SimpleMutableCell) extends LiftCell {
    def labelSVG : NodeSeq = <text class="orch-label" id={hashCode.toString}>{item.toString}</text>

    def toSVG : NodeSeq = {
      val myRect : NodeSeq = <rect x={x.toString} y={y.toString} rx="4" ry="4" width={width.toString} height={height.toString} stroke="black" stroke-width="2" fill="white"/>

      // I think if you make labelSVG and *element* you can use a scala method to change its attributes ...
      val labelX = x + width - labelWidth - internalPadding - strokeWidth
      val labelY = y + height - internalPadding - strokeWidth - (1.5 * strokeWidth)

      val myLabel : NodeSeq = <text class="orch-label" x={labelX.toString} y={labelY.toString} id={hashCode.toString}>{item.toString}</text>

      canopy match {
        case None => myRect ++ myLabel
        case Some(tree) => myRect ++ myLabel ++ <g>{tree.toList map (c => c.toSVG)}</g>
      }
    }

    def setLabelFromBBox(dict : Map[String, BBox]) = {
      labelWidth = dict(hashCode.toString).width
      labelHeight = dict(hashCode.toString).height 
    }

    def setLabelSize = ()
  }

  class SimpleEdge(val owner : complex.SimpleMutableCell) extends LiftEdge {
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

    def toSVG : NodeSeq = 
      <path d={pathString} stroke="black" stroke-width="2" fill="none" />

    // def renderPath =
    // {
    //   val startMove = new MoveTo(incomingX, incomingY)

    //   if (isVertical) {
    //     val vertLine = new VLineTo(outgoingY)
    //     getElements.setAll(List(startMove, vertLine))
    //   } else {
    //     val vertLine = new VLineTo(outgoingY - arcRadius)

    //     val arcTo = new ArcTo
    //     arcTo.setX(if (incomingX > outgoingX) (incomingX - arcRadius) else (incomingX + arcRadius))
    //     arcTo.setY(outgoingY)
    //     arcTo.setRadiusX(arcRadius)
    //     arcTo.setRadiusY(arcRadius)
    //     arcTo.setSweepFlag(incomingX > outgoingX)

    //     val horizLine = new HLineTo(outgoingX)

    //     getElements.setAll(List(startMove, vertLine, arcTo, horizLine))
    //   }
    // }


  }

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
}
