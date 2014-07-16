/**
  * WorksheetHandle.scala - A Trait for exporting worksheet operations
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.typechecker

import orchard.core.cell._
import orchard.core.complex._

trait WorksheetHandle { thisHandle =>

  def complex : MarkerComplex

  def selectAsBase(address : CellAddress) : Unit
  def selectCell(address : CellAddress) : Unit
  def deselectCell(address : CellAddress) : Unit

  def extrudeSelection : Unit
  def dropSelection : Unit

  class MarkerComplex(seed : NCell[ExpressionMarker])
      extends AbstractMutableComplex[ExpressionMarker](seed)
      with SelectableComplex[ExpressionMarker] { thisComplex =>

    type CellType = MarkerCell

    def handle : WorksheetHandle = thisHandle

    def newCell(item : ExpressionMarker) = new MarkerCell(item)

    class MarkerCell(var item : ExpressionMarker) extends AbstractMutableCell {

      def isNeutral : Boolean = item.isNeutral

    }

    override def selectAsBase(cell : MarkerCell) : Unit = {
      super.selectAsBase(cell)
      thisHandle.selectAsBase(cell.address)
    }

    override def select(cell : MarkerCell) : Unit = {
      super.select(cell)
      thisHandle.selectCell(cell.address)
    }

    override def deselect(cell : MarkerCell) : Unit = {
      super.deselect(cell)
      thisHandle.deselectCell(cell.address)
    }

  }

}
