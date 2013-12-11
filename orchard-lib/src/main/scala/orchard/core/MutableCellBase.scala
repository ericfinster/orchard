/**
  * MutableCellBase.scala - Base trait for cells which have mutability
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

trait MutableCellBase[C <: MutableCellBase[C, E], E <: MutableEdgeBase[C, E]] extends CellBase[C, E] { thisCell : C => 

  def canopy_=(c : Option[RoseTree[C, Int]]) : Unit
  def target_=(e : Option[E]) : Unit
  def sources_=(srcs : Option[Vector[E]])
  def container_=(c : Option[C])

}

trait MutableEdgeBase[C <: MutableCellBase[C, E], E <: MutableEdgeBase[C, E]] extends EdgeBase[C, E] { thisEdge : E => 

  def incoming_=(c : Option[C]) : Unit
  def outgoing_=(c : Option[C]) : Unit

}

