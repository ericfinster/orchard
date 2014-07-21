/**
  * WorksheetHandle.scala - A Trait for exporting worksheet operations
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.typechecker

import orchard.core.cell._
import orchard.core.util._
import orchard.core.complex._

trait WorksheetHandle { thisHandle =>

  def complex : MarkerComplex

  def selectAsBase(address : CellAddress) : Unit
  def selectCell(address : CellAddress) : Unit
  def deselectCell(address : CellAddress) : Unit

  def extrudeSelection : Unit
  def dropSelection : Unit

  class MarkerComplex(seed : NCell[ExpressionMarker])
      extends MutableSkeletalComplex[ExpressionMarker]
      with SelectableComplex[ExpressionMarker] { thisComplex =>

    type CellType = MarkerCell

    def handle : WorksheetHandle = thisHandle

    def newCell(item : ExpressionMarker) = new MarkerCell(item)

    class MarkerCell(var item : ExpressionMarker) extends MutableSkeletalCell {

      var canopy : Option[RoseTree[CellType, Int]] = None
      var target : Option[CellType] = None
      var sources : Option[Vector[CellType]] = None
      var container : Option[CellType] = None

      var incoming : Option[CellType] = None
      var outgoing : Option[CellType] = None

      // Umm ....
      var skeleton : NCell[CellType] = null

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
