/**
  * SelectableComplex.scala - A Gallery which allows for selection
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.complex

import scala.collection.mutable.Set
import scala.collection.mutable.HashSet

import orchard.core.util._

trait SelectableComplex[A] extends CellComplex[A] {

  var selectionBase : Option[CellType] = None
  val selectedCells : Set[CellType] = new HashSet

  def deselectAll : Unit = {
    // selectedCells foreach
    // (cell => cell.owner.emitToFaces(RequestCellDeselected))
    selectedCells.clear
    selectionBase = None
  }

  def deselect(cell : CellType) : Unit = ???

  def clearAndSelect(cell : CellType) : Unit = {
    deselectAll
    selectAsBase(cell)
  }

  def selectAsBase(cell : CellType) : Unit = {
    select(cell)
    selectionBase = Some(cell)
  }

  def isSelected(cell : CellType) : Boolean =
    selectedCells contains cell

  def selectionIsUnique : Boolean = 
    selectedCells.size == 1

  def trySelect(cell : CellType) : Boolean = {
    // This should be guaranteed by the cardinal structure
    val base = selectionBase.get
    val baseContainer = base.container.get

    if (cell.container.get != baseContainer)
      return false

    val candidates : Set[CellType] = new HashSet
    candidates add cell

    // Now look up a zipper to this guy
    val zipper : RoseZipper[CellType, Int] =
      new RoseZipper(baseContainer.canopy.get, Nil)
    var ptrOpt = zipper.lookup(cell).get.zipOnce

    // Step back through the zipper and look for the base selection
    while (ptrOpt != None) {
      val ptr = ptrOpt.get
      val testCell = ptr.focus.rootElement.get

      if (isSelected(testCell)) {
        // We're done!!
        candidates foreach (c => select(c))
        return true
      } else {
        candidates add testCell
        ptrOpt = ptr.zipOnce
      }
    }

    return false
  }

  def select(cell : CellType) : Unit = {
    // cell.owner.emitToFaces(RequestCellSelected)
    selectedCells add cell
  }

}
