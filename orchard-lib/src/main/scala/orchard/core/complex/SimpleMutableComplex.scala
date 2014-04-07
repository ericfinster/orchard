/**
  * SimpleMutableComplex.scala - A Simple Implementation of mutability
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.complex

import scala.collection.mutable.WeakHashMap

import orchard.core.cell._
import orchard.core.util._

import Util._

class SimpleMutableComplex[A](seed : NCell[A]) extends AbstractMutableComplex[A](seed) {

  type CellType = SimpleMutableCell

  //============================================================================================
  // COMPLEX IMPLEMENTATION
  //

  def newCell(item : A) = new SimpleMutableCell(item)

  //============================================================================================
  // CELL IMPLEMENTATION
  //

  class SimpleMutableCell(var item : A) extends AbstractMutableCell {
    override def toString = "Cell(" ++ item.toString ++ ")@" ++ hashCode.toString
  }

}
