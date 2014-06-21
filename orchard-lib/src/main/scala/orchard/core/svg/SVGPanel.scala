/**
  * SVGPanel.scala - A Panel which can render its contents to SVG
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.svg

import orchard.core.ui._
import orchard.core.complex._

import xml._

case class BBox(val x : Int, val y : Int, val width : Int, val height : Int)

trait SVGPanel[A] extends RenderingPanel[A] {

  override type CellType <: SVGCell
  override type EdgeType <: SVGEdge

  // We need a method which will return just the labels, and one which will 
  // return the whole rendered SVG.

  def labelProofSheet : NodeSeq = {
    var lps : NodeSeq = Seq.empty

    baseCell foreachCell (c => {
      lps ++= c.labelSVG
    })

    <g>{lps}</g>
  }

  abstract class SVGCell extends VisualCell { thisCell : CellType =>

    def labelSVG : NodeSeq
    def toSVG : NodeSeq

  }

  abstract class SVGEdge extends VisualEdge { thisEdge : EdgeType =>

    def toSVG : NodeSeq

  }

}

