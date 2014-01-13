/**
  * SimpleSVGPanel.scala - Simple implementation of an SVG Panel
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.svg

import orchard.core._
import xml._

class SimpleSVGPanel[A](val complex : SimpleMutableComplex[A], baseIndex : Int) extends SVGPanel[A] { thisPanel =>

  type CellType = SimpleSVGCell
  type EdgeType = SimpleSVGEdge

  type ComplexType = SimpleMutableComplex[A]

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

  // Okay, this class is nul.  We haven't implemented anything and I think you have
  // a better way.  Come back and clean it up later
  def setLabelSizes = ()

  def refresh = ()

  class SimpleSVGCell(val owner : complex.SimpleMutableCell) extends SVGCell {
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
  }

  class SimpleSVGEdge(val owner : complex.SimpleMutableCell) extends SVGEdge {
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
  }

  def newCell(owner : complex.SimpleMutableCell) : SimpleSVGCell = {
    val simpleCell = new SimpleSVGCell(owner)
    owner.registerPanelCell(thisPanel)(simpleCell)
    simpleCell
  }

  def newEdge(owner : complex.SimpleMutableCell) : SimpleSVGEdge = {
    val simpleEdge = new SimpleSVGEdge(owner)
    owner.registerPanelEdge(thisPanel)(simpleEdge)
    simpleEdge
  }

  //============================================================================================
  // UI INITIALIZATION
  //

  var baseCell : SimpleSVGCell = newCell(complex.baseCells(baseIndex))

  refreshPanelData
}
