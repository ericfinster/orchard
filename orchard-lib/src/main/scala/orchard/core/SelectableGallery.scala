/**
  * SelectableGallery.scala - A Gallery which can select cells
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

import scala.collection.JavaConversions._

import scala.collection.mutable.Set
import scala.collection.mutable.HashSet

import Util._

trait SelectableGallery[A] extends Gallery[A] {

  type GalleryCell = PanelType#CellType

  var selectionBase : Option[GalleryCell] = None
  val selectedCells : Set[GalleryCell] = new HashSet

  def deselectAll = {
    selectedCells foreach
    (cell => cell.owner.emitToFaces(RequestCellDeselected))
    selectedCells.clear
    selectionBase = None
  }

  def clearAndSelect(cell : GalleryCell) = {
    deselectAll
    selectAsBase(cell)
  }

  def selectAsBase(cell : GalleryCell) = {
    select(cell)
    selectionBase = Some(cell)
  }

  def isSelected(cell : GalleryCell) =
    selectedCells contains cell

  def selectionIsUnique : Boolean = 
    selectedCells.size == 1

  def trySelect(cell : GalleryCell) : Boolean = {
    // This should be guaranteed by the cardinal structure
    val base = selectionBase.force
    val baseContainer = base.container.force

    if (cell.container.force != baseContainer)
      return false

    val candidates : Set[GalleryCell] = new HashSet
    candidates add cell

    // Now look up a zipper to this guy
    val zipper : RoseZipper[GalleryCell, Int] =
      new RoseZipper(baseContainer.canopy.force, Nil)
    var ptrOpt = zipper.lookup(cell)
      .force("Lookup failed for selected cell.").zipOnce

    // Step back through the zipper and look for the base selection
    while (ptrOpt != None) {
      val ptr = ptrOpt.force
      val testCell = ptr.focus.rootElement.force("No root element?")

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

  def select(cell : GalleryCell) = {
    cell.owner.emitToFaces(RequestCellSelected)
    selectedCells add cell
  }

}


