/**
  * Worksheet.scala - Server side worksheet implementation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.checker

import orchard.core.cell._
import orchard.core.complex._

class Worksheet(seed : NCell[Polarity[Option[Expression]]]) extends AbstractWorksheet(seed) {

  type CellType = WorksheetCell
  type FrameworkType = Worksheet

  def newCell(item : Polarity[Option[Expression]]) : WorksheetCell = 
    new WorksheetCell(item)

  var topCell : WorksheetCell = 
    seed.regenerateFrom(ComplexGenerator).value

  def extract(cell : CellType) : Worksheet = 
    new Worksheet(cell.toNCell)

  def invertibilityLevel: Option[Int] = None
  def stabilityLevel: Option[Int] = None
  def unicityLevel: Option[Int] = None

  class WorksheetCell(item : Polarity[Option[Expression]]) extends AbstractWorksheetCell(item) {
  }

  def toMarkerComplex : MarkerComplex = 
    new MarkerComplex(
      topCell.skeleton map (cell => {
        cell.item match {
          case Positive => PositivePolarityMarker
          case Negative => NegativePolarityMarker
          case Neutral(None) => EmptyMarker(cell.isShell, cell.isExposedNook)
          case Neutral(Some(e)) => ReferenceMarker(e.name, e.styleString)
        }
      })
    )

}

object Worksheet {

  val emptyWorksheetCell : NCell[Polarity[Option[Expression]]] = 
    CardinalComplex(Object(None))

  def apply() : Worksheet = 
    new Worksheet(emptyWorksheetCell)

}
