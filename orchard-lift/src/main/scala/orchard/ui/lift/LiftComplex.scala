/**
  * LiftComplex.scala - A Generic Complex to start working with in Lift
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.lift

import orchard.core.cell._
import orchard.core.util._
import orchard.core.complex._

class LiftComplex[A](seed : NCell[A]) extends MutableSkeletalComplex[A] {

  type CellType = LiftCell

  def newCell(item : A) : LiftCell = new LiftCell(item)

  var topCell : LiftCell =
    seed.regenerateFrom(ComplexGenerator).value

  class LiftCell(var item : A) extends MutableSkeletalCell {

    var canopy : Option[RoseTree[LiftCell, Int]] = None
    var target : Option[LiftCell] = None
    var sources : Option[Vector[LiftCell]] = None
    var container : Option[LiftCell] = None
    
    var incoming : Option[LiftCell] = None
    var outgoing : Option[LiftCell] = None

    // Ummm ... really?
    var skeleton : NCell[CellType] = null

    def faces : Array[CellType] = ???
    def neighborhood : Array[CellType] = ???

  }

}
