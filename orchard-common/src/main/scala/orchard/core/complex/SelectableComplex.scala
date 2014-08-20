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
import ErrorM._

trait SelectableComplex[A] extends CellComplex[A] {

  var selectionBase : Option[CellType] = None
  val selectedCells : Set[CellType] = new HashSet

  def deselectAll : Unit = {
    for { c <- selectedCells } { deselect(c) }
    selectionBase = None
  }

  def deselect(cell : CellType) : Unit = {
    selectedCells remove cell
  }

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

  def selectRay(from : CellType, ray : Vector[Int]) : Error[Unit] = {
    if (ray.length > 0) {
      for {
        srcs <- fromOption(from.sources, "Cell has no sources")
        _ <- ensure(srcs.isDefinedAt(ray.head), "Invalid ray address")
        next <- fromOption(srcs(ray.head).incoming, "No incoming cell here")
        _ <- selectRay(next, ray.tail)
      } yield select(from)

    } else {
      succeed(select(from))
    }
  }

  def trySelect(cell : CellType) : Boolean = {
    // Is this okay?
    if (isSelected(cell)) return true

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
    while (ptrOpt.isSuccess) {
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
    selectedCells add cell
  }

  def descriptor : SelectionDescriptor = {
    val base = selectionBase map (_.address)
    val cells = selectedCells.toList map (_.address)
    SelectionDescriptor(base, cells)
  }

  def selectFromDescriptor(desc : SelectionDescriptor) : Error[Unit] = {
    deselectAll

    desc.base match {
      case None => succeed(())
      case Some(bAddr) => 
        for {
          newBase <- seek(bAddr)
          children <- sequence(desc.cells map (seek(_)))
          _ = selectAsBase(newBase)
          _ <- ensure(children forall (trySelect(_)), "A child failed to select")
        } yield ()
    }
  }
}
