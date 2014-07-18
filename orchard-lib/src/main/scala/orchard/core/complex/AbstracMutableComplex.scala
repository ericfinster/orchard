/**
  * AbstractMutableComplex.scala - A Abstract Implementation of mutability
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.complex

import scala.collection.mutable.HashMap

import orchard.core.ui._
import orchard.core.cell._
import orchard.core.util._

import Util._

abstract class AbstractMutableComplex[A](seed : NCell[A]) extends MutableComplex[A] {

  type CellType <: AbstractMutableCell

  //============================================================================================
  // COMPLEX IMPLEMENTATION
  //

  populateComplex(seed)

  //============================================================================================
  // CELL IMPLEMENTATION
  //

  abstract class AbstractMutableCell extends MutableCell { thisCell : CellType =>

    // Cell Data
    var canopy : Option[RoseTree[CellType, Int]] = None
    var target : Option[CellType] = None
    var sources : Option[Vector[CellType]] = None
    var container : Option[CellType] = None

    // Edge Data
    var incoming : Option[CellType] = None
    var outgoing : Option[CellType] = None

  }

}
