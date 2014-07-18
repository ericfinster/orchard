/**
  * SelectableGallery.scala - A Gallery which can select cells
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.ui

import scala.collection.JavaConversions._

import scala.collection.mutable.Set
import scala.collection.mutable.HashSet

import orchard.core.util._
import orchard.core.complex._

import Util._

trait SelectableGallery[A] extends Gallery[A] {

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
    val base = selectionBase.get
    val baseContainer = base.container.get

    if (cell.container.get != baseContainer)
      return false

    val candidates : Set[GalleryCell] = new HashSet
    candidates add cell

    // Now look up a zipper to this guy
    val zipper : RoseZipper[GalleryCell, Int] =
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

  def select(cell : GalleryCell) = {
    cell.owner.emitToFaces(RequestCellSelected)
    selectedCells add cell
  }

}


