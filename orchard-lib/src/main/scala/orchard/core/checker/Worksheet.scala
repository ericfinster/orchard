/**
  * Worksheet.scala - Server side worksheet implementation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.checker

import orchard.core.cell._
import orchard.core.util._
import orchard.core.complex._

class Worksheet(seed : NCell[WorksheetMarker]) 
    extends MutableSkeletalComplex[WorksheetMarker] 
    with SelectableComplex[WorksheetMarker] {
  thisWorksheet =>

  type CellType = WorksheetCell

  var topCell : WorksheetCell =
    seed.regenerateFrom(ComplexGenerator).value

  def newCell(marker : WorksheetMarker) = new WorksheetCell(marker)

  class WorksheetCell(var item : WorksheetMarker) extends MutableSkeletalCell {

    var canopy : Option[RoseTree[WorksheetCell, Int]] = None
    var target : Option[WorksheetCell] = None
    var sources : Option[Vector[WorksheetCell]] = None
    var container : Option[WorksheetCell] = None
    
    var incoming : Option[WorksheetCell] = None
    var outgoing : Option[WorksheetCell] = None

    // Ummm ... really?
    var skeleton : NCell[CellType] = null

  }

}

object Worksheet {

  val emptyWorksheetCell : NCell[WorksheetMarker] = 
    Object(EmptyMarker(true, false)).glob(
      PositivePolarityMarker, 
      NegativePolarityMarker
    )

  def apply() : Worksheet = 
    new Worksheet(emptyWorksheetCell)

}
