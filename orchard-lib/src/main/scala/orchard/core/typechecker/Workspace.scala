/**
  * Workspace.scala - An encapsulation of a type checker allowing expression manipulation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.typechecker

import orchard.core.cell._
import orchard.core.complex._

class Workspace extends ErrorMonad {

  private val checker = new Checker

  def newWorksheet(name : String) : Error[WorksheetHandle] =
    succeed(new Worksheet(CardinalComplex(Object(None))))

  class Worksheet(seed : NCell[Polarity[Option[Expression]]]) 
      extends AbstractWorksheet(seed) with WorksheetHandle { thisWorksheet =>

    type CellType = WorksheetCell
    type FrameworkType = Worksheet

    def newCell(item : Polarity[Option[Expression]]) : WorksheetCell = 
      new WorksheetCell(item)

    def extract(cell : WorksheetCell) : Worksheet =
      new Worksheet(cell.skeleton.map (_.item))

    def stabilityLevel : Option[Int] = None
    def invertibilityLevel : Option[Int] = None
    def unicityLevel : Option[Int] = None

    class WorksheetCell(item : Polarity[Option[Expression]]) extends AbstractWorksheetCell(item)

    //============================================================================================
    // HANDLE IMPLEMENTATION
    //

    def complex : MarkerComplex = {

      val markerNCell = topCell.skeleton map (cell => {
        cell.item match {
          case Positive => PositivePolarityMarker
          case Negative => NegativePolarityMarker
          case Neutral(None) => EmptyMarker(false, false)
          case Neutral(Some(expr)) => ReferenceMarker(expr.name, expr.styleString)
        }
      })

      new MarkerComplex(markerNCell)

    }

    def selectAsBase(address : CellAddress) : Unit = 
      for {
        cell <- thisWorksheet.seek(address)
      } {
        thisWorksheet.selectAsBase(cell)
      }

    def selectCell(address : CellAddress) : Unit =
      for {
        cell <- thisWorksheet.seek(address)
      } {
        thisWorksheet.select(cell)
      }

    def deselectCell(address : CellAddress) : Unit = 
      for {
        cell <- thisWorksheet.seek(address)
      } {
        thisWorksheet.deselect(cell)
      }

    def extrudeSelection : Unit = 
      emptyExtrusion

    def dropSelection : Unit = 
      emptyDrop

  }
}
