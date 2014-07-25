/**
  * MutableComplex.scala - A Mutable Cell Complex
  * 
  * @author Eric Finster
  * @version 0.1
  */

package orchard.core.complex

import orchard.core.cell._
import orchard.core.util._

import Util._
import ErrorM._

trait MutableComplex[A] extends CellComplex[A] { thisComplex =>

  type CellType <: MutableCell

  def topCell_=(cell : CellType) : Unit

  //============================================================================================
  // COMPLEX ROUTINES
  //

  def glob(globValue : A, targetValue : A) : CellType = {

    val newSource = topCell

    val newGlob = newCell(globValue)
    val newTarget = newCell(targetValue)

    newTarget.target = newSource.target
    newTarget.sources = newSource.sources

    newTarget.canopy = 
      for { srcs <- newSource.sources }
      yield {
        var srcIndex = -1

        val branches = srcs map
          (src => {
            srcIndex += 1
            Rose(srcIndex)
          })

        Branch(newSource, branches)
      }

    newSource.outgoing = Some(newGlob)
    newSource.container = Some(newTarget)

    newTarget.incoming = Some(newGlob)

    newGlob.target = Some(newTarget)
    newGlob.sources = Some(Vector(newSource))

    topCell = newGlob
    newGlob
  }

  // def stablyAppend(complex : CellComplex[A]) = {

  //   val cellMap = HashMap.empty[complex.CellType, CellType]

  //   // Okay, we generate all the new cells at once and we will
  //   // then go on to set the semantic data
  //   complex forAllCells (cell => {
  //     cellMap(cell) = newCell(cell.item)
  //   })

  //   def cloneFromCell(srcCell : complex.CellType, tgtCell : CellType) = {
  //     tgtCell.canopy = for { tree <- srcCell.canopy } yield {
  //       tree.map((c => cellMap(c)), (i => i))
  //     }

  //     tgtCell.target = for { tgt <- srcCell.target } yield { cellMap(tgt) }
  //     tgtCell.sources = for { srcs <- srcCell.sources } yield { srcs map (src => cellMap(src)) }
  //     tgtCell.container = for { cntnr <- srcCell.container } yield { cellMap(cntnr) }

  //     tgtCell.loops = srcCell.loops map (loop => cellMap(loop))

  //     tgtCell.incoming = for { inc <- srcCell.incoming } yield { cellMap(inc) }
  //     tgtCell.outgoing = for { out <- srcCell.outgoing } yield { cellMap(out) }
  //   }

  //   // Now copy all the data into this complex
  //   complex forAllCells (cell => { cloneFromCell(cell, cellMap(cell)) })

  //   // Fix up the objects to have the correct sources and targets
  //   complex(0) foreachCell (cell => {
  //     val theCell = cellMap(cell)
  //     theCell.sources = topCell.sources
  //     theCell.target = topCell.target

  //     // BUG!!! The previous dimensions cells must all have their incoming and outgoing set????
  //     if (theCell.isExternal) {
  //       for { srcs <- theCell.sources } {
  //         srcs foreach (src => src.outgoing = Some(theCell))
  //       }

  //       for { tgt <- theCell.target } {
  //         tgt.incoming = Some(theCell)
  //       }
  //     }

  //   })

  //   // First set the correct base cell for the objects
  //   myBaseCells(myBaseCells.length - 1) = cellMap(complex(0))

  //   // Now finish off the rest 
  //   complex.baseCells.tail foreach (base => appendBaseCell(cellMap(base)))

  //   // Now rigidify the cells
  //   complex forAllCells (cell => cellMap(cell).rigidify)
  // }

  //============================================================================================
  // CELL IMPLEMENTATION
  //

  trait MutableCell extends ComplexCell 
    with MutableCellBase[CellType, CellType]
    with MutableEdgeBase[CellType, CellType] { thisCell : CellType =>

    //============================================================================================
    // MUTATION
    //

    def item_=(newItem : A) : Unit

    def dumpInfo : Unit = {
      println("Dumping semantic info for " ++ thisCell.toString)

      sources match {
        case None => println("No sources")
        case Some(srcs) => println("Source list: " ++ srcs.toString)
      }

      canopy match {
        case None => println("Cell is external")
        case Some(tree) => println("Leaf permutation: " ++ tree.leaves.toString)
      }
    }

    def insertComposite(compositeValue : A, universalValue : A,
                        location : RoseZipper[CellType, Int],
                        selector : CellType => Boolean) : (CellType, CellType) = {

      // This is the main routine for modifying a mutable cell by inserting
      // a composite and its universal cell.

      // Note: since you have moved this whole setup inside MutableComplex here, maybe you should grab a
      // list of the affected cells *first*, before you start messing with shit and you already know that
      // everything is in a decent state.  Then you can go ahead and run through the list at the end,
      // once you know that the lower dimensional information has been corrected.

      // Make sure we have all the available information before proceeding
      val myFiller = incoming.get
      val myFillerContainer = myFiller.container.get
      val topFiller = myFiller.outgoing.get

      val compositeCell = newCell(compositeValue)
      val universalCell = newCell(universalValue)

      // First step: clip the canopy to the bounds determined by the selector

      var localIndex : Int = -1

      def clip(loc : RoseTree[CellType, Int]) : (RoseTree[CellType, Int], Vector[RoseTree[CellType, Int]]) = {
        loc match {
          case Rose(idx) => {
            localIndex += 1
            (Rose(localIndex), Vector(Rose(idx)))
          }
          case Branch(value, branches) => {
            if (selector(value)) {
              // This cell is part of the selection
              val (newBranches, newClippings) = (branches map (b => clip(b))).unzip
              (Branch(value, newBranches), newClippings.flatten)
            } else {
              // This cell is not part of the selection (i.e. it's on the boundary)
              localIndex += 1
              (Rose(localIndex), Vector(Branch(value, branches)))
            }
          }
        }
      }

      // Clip the current canopy to the selection
      val (compositeCanopy, clippings) = clip(location.focus)

      // Get all the sources for the new composite cell
      val compositeSources = optSwitchVect(clippings map (branch => edgeAt(branch)))

      // Update the container of all cells in the newCanopy
      compositeCanopy foreachCell (cell => cell.container = Some(compositeCell))

      // Build the new canopy for this cell
      val newCanopy = location.setFocus(Branch(compositeCell, clippings)).zip

      // Set up all the data
      compositeCell.canopy = Some(compositeCanopy)             // OK: the selection
      compositeCell.target = edgeAt(location.focus)            // OK: Should be the target of the tree
      compositeCell.sources = compositeSources                 // OK: Target of all clipped entities
      compositeCell.container = Some(thisCell)                 // OK: it's contained in this cell

      // Update the canopy of this cell.  All other fields are unchanged.
      canopy = Some(newCanopy)

      // Now, we move on to the next dimension where we look for the filling cell
      val ptr = RoseZipper(myFillerContainer.canopy.get, Nil).lookup(myFiller).get

      val (fillerSources, universalSources) = (newCanopy.nodeVector, compositeCanopy.nodeVector)

      // Now let's put the universal cell into place
      val newFocus =
        ptr.focus match {
          case Branch(fc, branches) => {
              // Return the new focus with the new cell in its correct place and the
              // branches adjusted accordingly ...

              val universalBranches = universalSources map
                (src => {
                  val branch = (branches find (b => myFillerContainer.edgeAt(b).get == src)).get
                  myFillerContainer.edgeAt(branch).get.outgoing = Some(universalCell)
                  branch
                })

              val fillerBranches = fillerSources map
                (src => {
                  if (src == compositeCell) {
                    Branch(universalCell, universalBranches)
                  } else {
                    (branches find (b => myFillerContainer.edgeAt(b).get == src)).get
                  }
                })

              Branch(fc, fillerBranches)
            }
          case _ => throw new IllegalArgumentException("This can't happen.")
        }

      val myFillerContainerCanopy = ptr.setFocus(newFocus).zip
      myFillerContainer.canopy = Some(myFillerContainerCanopy)

      // Set up the new cell and edge
      compositeCell.incoming = Some(universalCell)
      compositeCell.outgoing = Some(myFiller)

      universalCell.target = Some(compositeCell)
      universalCell.sources = Some(universalSources)
      universalCell.container = Some(myFillerContainer)

      myFiller.sources = Some(fillerSources)

      universalCell.outgoing = Some(topFiller)
      topFiller.sources = Some(myFillerContainerCanopy.nodeVector)

      // val affectedDimensions = Range(dimension, thisComplex.dimension + 1)
      // affectedDimensions foreach (d => {
      //   val theBase = thisComplex(d)
      //   theBase.rigidify
      //   // theBase.cellPanels.foreach (panel => panel.refreshPanelData)
      // })

      (compositeCell, universalCell)
    }

  }
}
