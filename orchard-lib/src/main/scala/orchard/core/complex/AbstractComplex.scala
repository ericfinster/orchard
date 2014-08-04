/**
  * AbstractComplex.scala - A minimal abstract complex implementation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.complex

import orchard.core.cell._
import orchard.core.util._

abstract class AbstractComplex[A](seed : NCell[A]) extends MutableSkeletalComplex[A] {

  type CellType <: AbstractComplexCell

  var topCell : CellType =
    seed.regenerateFrom(ComplexGenerator).value

  abstract class AbstractComplexCell extends MutableSkeletalCell { 
    thisCell : CellType =>

      var canopy : Option[RoseTree[CellType, Int]] = None
      var target : Option[CellType] = None
      var sources : Option[Vector[CellType]] = None
      var container : Option[CellType] = None

      var incoming : Option[CellType] = None
      var outgoing : Option[CellType] = None

      // Umm ....
      var skeleton : NCell[CellType] = null

  }

}
