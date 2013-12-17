/**
  * CellBase.scala - Base traits for mutable cells
  * 
  * @author Eric Finster
  * @version 0.1
  */

package orchard.core

import Util._

trait CellBase[C <: CellBase[C, E], E <: EdgeBase[C, E]] { thisCell : C =>

  def canopy : Option[RoseTree[C, Int]]
  def target : Option[E]
  def sources : Option[Vector[E]]
  def container : Option[C]

  def isExternal = canopy == None
  def isObject = target == None
  def isBase = container == None

  def isLoop : Boolean = 
    sources match {
      case None => false
      case Some(srcs) =>
        target match {
          case None => false
          case Some(tgt) => {
            if (srcs.length == 1) {
              if (srcs.head == tgt) true else false
            } else false
          }
        }
    }

  def sourceCount = 
    sources match {
      case None => 0
      case Some(srcs) => srcs.length
    }

  def baseContainer : C =
    container match {
      case None => this
      case Some(c) => c.baseContainer
    }

  def foreachCell(action : C => Unit) : Unit = {
    action(this)

    canopy match {
      case None => ()
      case Some(tree) => 
        tree.foreachCell(c => c foreachCell action)
    }
  }

  def edgeAt(ptr : RoseTree[C, Int]) : Option[E] = {
    ptr match {
      case Rose(idx) => for { srcs <- sources } yield srcs(idx)
      case Branch(value, branches) => value.target
    }
  }
}

trait EdgeBase[C <: CellBase[C, E], E <: EdgeBase[C, E]] { thisEdge : E =>

  def incoming : Option[C]
  def outgoing : Option[C]

  def isLeaf = incoming == None
  def isTarget = outgoing == None

  def foreachEdge(action : E => Unit) : Unit = {
    for { cell <- incoming } {
      for { srcs <- cell.sources } {
        srcs foreach (src => src foreachEdge action)
      }
    }

    action(this)
  }
}
