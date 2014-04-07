/**
  * MutablePanel.scala - A Panel which reacts to mutability events
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.ui

import orchard.core.complex.MutableComplex

trait MutablePanel[A] extends Panel[A] { thisPanel =>

  override type CellType <: MutablePanelCell
  override type EdgeType <: MutablePanelEdge

  override type ComplexType <: MutableComplex[A]

  trait MutablePanelCell extends PanelCell { thisCell : CellType =>

  }

  trait MutablePanelEdge extends PanelEdge { thisEdge : EdgeType =>

  }

}
