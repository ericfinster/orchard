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

trait SVGPanel[A] extends SizeablePanel[A] {

  override type CellType <: SVGCell
  override type EdgeType <: SVGEdge

  abstract class SVGCell extends SizeableCell { thisCell : CellType =>

  }

  abstract class SVGEdge extends SizeableEdge { thisEdge : EdgeType =>


  }

}

