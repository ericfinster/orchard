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

  // What do we need to do?  We need to create a kind of attribute list which stores
  // the gallery's address and panel info, etc.

  trait SVGCell extends SizeableCell { thisCell : CellType =>

  }

  trait SVGEdge extends SizeableEdge { thisEdge : EdgeType =>

  }

}

