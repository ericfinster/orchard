/**
  * MutableComplex.scala - A Mutable Cell Complex
  * 
  * @author Eric Finster
  * @version 0.1
  */

package orchard.core

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.WeakHashMap

import Util._

trait MutableComplex[A] extends CellComplex[A] {

  type CellType <: MutableCell

  //============================================================================================
  // COMPLEX ROUTINES
  //

  def appendBaseCell(cell : CellType) : Unit = myBaseCells += cell
  def setBaseCell(i : Int, cell : CellType) = myBaseCells(i) = cell

  def glob(globValue : A, targetValue : A) : CellType = {
    val newGlob = newCell(globValue)
    val newTarget = newCell(targetValue)

    topCell.globEnclose(newTarget)
    topCell.emit(new ChangeEvents.GlobEncloseEvent(newTarget))

    topCell.outgoing = Some(newGlob)
    topCell.container = Some(newTarget)

    newTarget.incoming = Some(newGlob)

    newGlob.target = Some(newTarget)
    newGlob.sources = Some(topCell :: Nil)

    val globCell = topCell.skeleton.glob(newTarget, newGlob)

    newGlob.skeleton = globCell
    newTarget.skeleton = globCell.target

    setBaseCell(topCell.dimension, newTarget)
    appendBaseCell(newGlob)
    emit(ComplexExtended)

    newGlob
  }

  //============================================================================================
  // CHANGE EVENTS
  //

  object ChangeEvents {
    sealed trait ChangeEvent extends CellEvent

    case class SproutEvent(newEdge : CellType,
      newSources : List[CellType]) extends ChangeEvent

    case class SpawnEvent(oldCell : CellType,
      newCell : CellType,
      newEdge : CellType,
      oldCellSources : List[CellType],
      newCellSources : List[CellType]) extends ChangeEvent

    case class EncloseEvent(enclosingCell : CellType,
      location : RoseZipper[CellType, Int],
      selector : CellType => Boolean) extends ChangeEvent

    case class GlobEncloseEvent(newTarget : CellType) extends ChangeEvent

    case class ItemChangedEvent(oldItem : A) extends ChangeEvent
  }

  //============================================================================================
  // CELL IMPLEMENTATION
  //

  trait MutableCell extends ComplexCell { thisCell : CellType =>

    //============================================================================================
    // MUTATION
    //

    import ChangeEvents._

    def item_=(newItem : A) : Unit

    def insertComposite(compositeValue : A, universalValue : A,
                        location : RoseZipper[CellType, Int],
                        selector : CellType => Boolean) : Unit =
    {
      // This is the main routine for modifying a mutable cell by inserting
      // a composite and its universal cell.

      val fillingCell = incoming.force("Need a filling cell")
      val fillingContainer = fillingCell.container.force("Filling cell needs a container")
      val topFiller = fillingCell.outgoing.force("Need a top cell for the new leaf")

      val compositeCell = newCell(compositeValue)
      val universalCell = newCell(universalValue)

      // Enclose the selected cells with a new target
      val (fillerSources, universalSources) =
        enclose(compositeCell, location, selector)


      // Spawn a new external cell in the next dimension
      val newLeaves = fillingContainer.spawn(fillingCell,
                                             universalCell,
                                             compositeCell,
                                             fillerSources.toList,
                                             universalSources.toList)

      // Finally, add a new leaf for the new external cell
      topFiller.sprout(universalCell, newLeaves.toList)

      // Now fix up the skeletons
      compositeCell.rebuildSkeleton
      universalCell.rebuildSkeleton

      fillingCell.neighborhood foreach
        (neighbor => {
          neighbor.rebuildSkeleton
        })

      // Okay ... the idea is that, at least for cell complexes, we can do this using the
      // same method that we use to rebuild the skeleton: a traverse of the target which
      // picks out the source cells we are looking for.  The result is a tree in the source
      // order, and when we flatten it, that will be the correct order for the sources.

      // This I think, moreover, is the correct replacement for the "comb" function you
      // are using currently.

      // And pass on the events to any listeners
      emit(new EncloseEvent(compositeCell, location, selector))

      fillingContainer.emit(new SpawnEvent(fillingCell, universalCell, compositeCell, 
        fillerSources.toList, universalSources.toList))

      topFiller.emit(new SproutEvent(universalCell, newLeaves.toList))
    }

    // Ech.  This is a mess and should be redone.
    def nskel : NCell[CellType] =
    {
      if (debug) println("Rebuilding skeleton for " ++ this.toString)

      if (isObject) {
        Object(this)
      } else {
        val myTarget = target.force("Higher dimensional cell is missing a target.")
        val myTargetSkel = myTarget.skeleton  // Why do we recalculate the skeleton for the lower dimensional faces?
        var remainingSources = sources.force("Higher dimensional cell has no sources.")

        if (isLoop) {
          Composite(this, myTargetSkel.corolla, myTarget)
        } else if (isArrow) {
          val srcObj = remainingSources.head.skeleton

          srcObj.ev match {
            case Left(ev) => {
              implicit val isZero = ev
              Composite(this, Seed(srcObj), myTarget)
            }
            case _ => throw new IllegalArgumentException("Source has wrong dimension.")
          }
        } else {
          myTargetSkel.ev match {
            case Left(ev) => {
              throw new IllegalArgumentException("Target has wrong dimension.")
            }
            case Right(ev) => {
              type D = myTargetSkel.dim.Self

              implicit val hasPred : HasPred[D] = ev

              def searchCell(cell : CellType, srcs : Array[CellTree[D, CellType]])
                  : CellTree[D, CellType] = {

                if (debug) {
                  println("Searching cell: " ++ cell.toString)
                  println("Sources are: " ++ (srcs map (_.toString)).toList.toString)
                }

                def traverseShell(tree : RoseTree[CellType, Int]) : CellTree[D, CellType]  = {
                  tree match {
                    case Rose(idx) => srcs(idx)
                    case Branch(mcell, branches) => {
                      val newBranches = branches map (branch => traverseShell(branch))

                      remainingSources match {
                        case Nil => newBranches.head  // I think this can only happen for a loop ...
                        case s :: ss => {
                          if (mcell == s) {
                            remainingSources = ss

                            if (debug) {
                              println("About to graft: " ++ mcell.toString)
                              println("New branches are: " ++ (newBranches map (_.toString)).toString)
                              println("Cell has sources: " ++ (mcell.sources.force map (_.toString)).toString)
                              println("Skeleton: " ++ mcell.skeleton.toString)
                            }

                            Graft(mcell.skeleton.cell.asInstanceOf[Cell[D, CellType]], newBranches)
                          } else {
                            searchCell(mcell, newBranches.toArray)
                          }
                        }
                      }
                    }
                  }
                }

                traverseShell(cell.shell.force("Missing shell"))
              }


              val tgtSources : List[CellTree[D, CellType]] =
                myTarget.sources.force map
                  (src => Leaf(src.skeleton.cell.asInstanceOf[Cell[D#Pred, CellType]]))

              val myCellTree = searchCell(myTarget, tgtSources.toArray)
              Composite(this, myCellTree, myTarget)
            }
          }
        }
      }
    }

    def rebuildSkeleton = { skeleton = nskel }
  }
}
