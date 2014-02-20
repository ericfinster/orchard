/**
  * MutableComplex.scala - A Mutable Cell Complex
  * 
  * @author Eric Finster
  * @version 0.1
  */

package orchard.core

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.WeakHashMap

import Util._

trait MutableComplex[A] extends CellComplex[A] { thisComplex =>

  type CellType <: MutableCell

  //============================================================================================
  // COMPLEX ROUTINES
  //

  protected val myBaseCells : ListBuffer[CellType] = new ListBuffer

  def baseCells : Vector[CellType] = myBaseCells.toVector

  def populateComplex(seed : NCell[A]) = 
    myBaseCells ++= seed.regenerateFrom(MutableComplexGenerator).value.targets

  def appendBaseCell(cell : CellType) : Unit = myBaseCells += cell
  def setBaseCell(i : Int, cell : CellType) = myBaseCells(i) = cell

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

    val globCell = newSource.skeleton.glob(newTarget, newGlob)

    newGlob.skeleton = globCell
    newTarget.skeleton = globCell.target

    setBaseCell(newSource.dimension, newTarget)
    appendBaseCell(newGlob)

    newSource.cellPanels foreach (panel => {
      val newPanelCell = panel.newCell(newTarget.asInstanceOf[panel.complex.CellType])
      panel.baseCell = newPanelCell
      panel.refreshPanelData
    })

    emit(ComplexExtended)

    newGlob
  }

  def stablyAppend(complex : CellComplex[A]) = {

    val cellMap = HashMap.empty[complex.CellType, CellType]

    // Okay, we generate all the new cells at once and we will
    // then go on to set the semantic data
    complex forAllCells (cell => {
      cellMap(cell) = newCell(cell.item)
    })

    def cloneFromCell(srcCell : complex.CellType, tgtCell : CellType) = {
      tgtCell.canopy = for { tree <- srcCell.canopy } yield {
        tree.map((c => cellMap(c)), (i => i))
      }

      tgtCell.target = for { tgt <- srcCell.target } yield { cellMap(tgt) }
      tgtCell.sources = for { srcs <- srcCell.sources } yield { srcs map (src => cellMap(src)) }
      tgtCell.container = for { cntnr <- srcCell.container } yield { cellMap(cntnr) }

      tgtCell.loops = srcCell.loops map (loop => cellMap(loop))

      tgtCell.incoming = for { inc <- srcCell.incoming } yield { cellMap(inc) }
      tgtCell.outgoing = for { out <- srcCell.outgoing } yield { cellMap(out) }
    }

    // Now copy all the data into this complex
    complex forAllCells (cell => { cloneFromCell(cell, cellMap(cell)) })

    // Fix up the objects to have the correct sources and targets
    complex(0) foreachCell (cell => {
      val theCell = cellMap(cell)
      theCell.sources = topCell.sources
      theCell.target = topCell.target
    })

    // First set the correct base cell for the objects
    myBaseCells(myBaseCells.length - 1) = cellMap(complex(0))

    // Now finish off the rest 
    complex.baseCells.tail foreach (base => appendBaseCell(cellMap(base)))

    // Finally, we need to rebuild all the skeletons ...
    // Annoyingly, I have the rigidify and skeleton update functions tied to
    // each other, even though they are really logically indepdendent.
    // We don't really want to rigidify, since this rebuilds the entire canopy,
    // and we already know that its state is just fine.  So I think we need to
    // split that part out so that we can be more efficient.

    // The above makes the following extremely silly, but nonetheless, I think correct:
    complex forAllCells (cell => cellMap(cell).rigidify)
  }

  //============================================================================================
  // CHANGE EVENTS
  //

  object ChangeEvents {
    sealed trait ChangeEvent extends CellEvent
    case class ItemChangedEvent(oldItem : A) extends ChangeEvent
    case class CompositeInsertionEvent(composite : CellType, universal : CellType) extends ChangeEvent
  }

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
      val myFiller = incoming.force("Need a filling cell")
      val myFillerContainer = myFiller.container.force("Filling cell needs a container")
      val topFiller = myFiller.outgoing.force("Need a top cell for the new leaf")

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
      val ptr = RoseZipper(myFillerContainer.canopy.get, Nil).lookup(myFiller).force("Failed to find the filler!")

      val (fillerSources, universalSources) = (newCanopy.nodeVector, compositeCanopy.nodeVector)

      // Now let's put the universal cell into place
      val newFocus =
        ptr.focus match {
          case Branch(fc, branches) => {
              // Return the new focus with the new cell in its correct place and the
              // branches adjusted accordingly ...

              val universalBranches = universalSources map
                (src => {
                  val branch = (branches find (b => myFillerContainer.edgeAt(b).force == src)).force("No branch found for this edge.")
                  myFillerContainer.edgeAt(branch).force.outgoing = Some(universalCell)
                  branch
                })

              val fillerBranches = fillerSources map
                (src => {
                  if (src == compositeCell) {
                    Branch(universalCell, universalBranches)
                  } else {
                    (branches find (b => myFillerContainer.edgeAt(b).force == src)).force("No branch found for this edge.")
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

      val affectedDimensions = Range(dimension, thisComplex.dimension + 1)
      affectedDimensions foreach (d => {
        val theBase = thisComplex(d)
        theBase.rigidify
        theBase.cellPanels.foreach (panel => panel.refreshPanelData)
      })

      thisComplex.emit(ChangeEvents.CompositeInsertionEvent(compositeCell, universalCell))

      (compositeCell, universalCell)
    }

    // After insertion of a new cell, the cell state variables are left in a dirty state. This
    // routine fixes them using that 1) the lower dimensional information is still correct and
    // 2) there is no permutation at external cells.

    def rigidify : Unit = {
      val dim = Nats.fromInt(dimension)

      dim.asInstanceOf[dim.Self] match {
        case IsZero(ev) => {
          implicit val isZero : IsZero[dim.Self] = ev

          // If we are looking at an object, there are no sources or targets and the only
          // thing to do is update the skeleton as so:
          canopy match {
            case None => ()
            case Some(tree) => {
              tree.rootElement.get.rigidify
            }
          }

          skeleton = Object(thisCell)
        }
        case HasPred(ev) => {
          implicit val hasPred : HasPred[dim.Self] = ev

          // Here we are going to reorder the sources, set the new canopy (which must have its
          // leaves set to match the correctly ordered sources) and update the skeleton

          val mySources : Vector[CellType] = sources.get // Error! this may miss something!
          val myTarget : CellType = target.get

          val myExtendedCanopy = 
            for { cnpy <- canopy } 
            yield {

                def rebuildCanopy(t : RoseTree[CellType, Int]) : RoseTree[CellType, CellType] =
                  t match {
                    case Rose(idx) => Rose(mySources(idx))
                    case Branch(cell, branches) => {

                      // First do this to the cell, so that the orders are correct
                      cell.rigidify

                      // We are going to twist so that we are in the order which agrees with
                      // the sources of the previously rigidified cell.  The one hiccup is that
                      // we may have added a source (as in the new source for "topFiller" in the
                      // insertion algorithm) and so if the match fails, we just save this source
                      // for later. (I think this will successively push it forward into all the
                      // containers ....)

                      val newBranches = cell.sources.get map (src => {
                        val branchOpt = branches find (b => edgeAt(b).get == src)

                        branchOpt match {
                          case None => Rose(src)
                          case Some(br) => rebuildCanopy(br)
                        }
                      })
                      
                      // Now regraft the tree with the twisted branches
                      Branch(cell, newBranches)
                    }
                  }

              rebuildCanopy(cnpy)
            }

          val myExtendedSources : Vector[CellType] = 
            myExtendedCanopy match {
              case None => mySources
              case Some(ecpy) => ecpy.leaves
            }

          val mySrcTree : RoseTree[CellType, Int] = myTarget.selectedCanopy(myExtendedSources)
          val myCorrectedSources : Vector[CellType] = mySrcTree.nodeVector

          sources = Some(myCorrectedSources)
          canopy = myExtendedCanopy map (cpy => {
            cpy map ((c => c), (l => myCorrectedSources.indexOf(l)))
          })

          // Next we are going to fix the skeleton, which requires another match on dimension ....

          type SrcDim = dim.Self#Pred
          val srcDim : SrcDim = Nats.getPred(dim.asInstanceOf[dim.Self])

          srcDim match {
            case IsZero(ev) => {
              implicit val srcIsZero : IsZero[SrcDim] = ev

              skeleton = Composite(thisCell, Seed(Object(mySrcTree.rootElement.get)), myTarget)
            }
            case HasPred(ev) => {
              implicit val srcHasPred : HasPred[SrcDim] = ev

              val mySrcRoseTree = mySrcTree map ((c => c.skeleton.cell.asInstanceOf[Cell[SrcDim, CellType]]),
                                                 (i => ((myTarget.sources.get)(i)).skeleton.cell.asInstanceOf[Cell[SrcDim#Pred, CellType]]))

              skeleton = Composite(thisCell, CellTree.fromRoseTree(mySrcRoseTree), myTarget)
            }
          }
        }
      }
    }

    // Extract the tree formed by successively passing to canopies and stopping when
    // the cell is in the list of sources
    def selectedCanopy(sources : Vector[CellType]) : RoseTree[CellType, Int] = {

      def verticalTrace(cell : CellType, lvs : Vector[RoseTree[CellType, Int]]) : RoseTree[CellType, Int] = {
        if (sources contains cell) {
          Branch(cell, lvs)
        } else {

            def horizontalTrace(tr : RoseTree[CellType, Int]) : RoseTree[CellType, Int] = 
              tr match {
                case Rose(idx) => if (cell.isObject) { Rose(idx) } else lvs(idx)
                case Branch(hCell, hBranches) => {
                  verticalTrace(hCell, hBranches map horizontalTrace)
                }
              }

          horizontalTrace(cell.canopy.get)
        }
      }

      val startLeaves = Range(0, sourceCount) map (i => Rose(i))
      verticalTrace(thisCell, startLeaves.toVector)
    }
  }

  //============================================================================================
  // COMPLEX GENERATION
  //

  object MutableComplexGenerator extends CellRegenerator[A, CellType] {

    def generateObject[D <: Nat : IsZero](value : A) : Cell[D, CellType] = {
      val newObj = newCell(value)
      val newObjSkeleton = ObjectCell(newObj)
      newObj.skeleton = newObjSkeleton
      newObjSkeleton
    }

    def generateCell[D <: Nat : HasPred](cellValue : A,
                                         srcs : CellTree[D#Pred, CellType],
                                         tgtValue : A) : Cell[D, CellType] = {

      val thisMutableCell = newCell(cellValue)
      val tgtMutableCell = newCell(tgtValue)

      thisMutableCell.canopy = None
      thisMutableCell.target = Some(tgtMutableCell)
      thisMutableCell.sources = Some(srcs.cells map
                                       (src => {
                                          src.value.outgoing = Some(thisMutableCell)
                                          src.value
                                        }))

      // We know the incoming cells
      thisMutableCell.incoming = None
      tgtMutableCell.incoming = Some(thisMutableCell)

      var curIdx : Int = -1
      val perm = srcs.inversePerm

      // Yeah, um, this is not good.  What you do is simply flatten the leaves
      // into a list.  But they should be put in the correct order based on flattening.

      srcs.dimension match {
        case IsZero(ev) => {
          implicit val isZero : IsZero[D#Pred] = ev

          val theObject = srcs.cells(0).value
          theObject.container = Some(tgtMutableCell)

          tgtMutableCell.canopy = Some(Branch(srcs.cells(0).value, Vector(Rose(0))))
          tgtMutableCell.target = None
          tgtMutableCell.sources = None

        }
        case HasPred(ev) => {
          implicit val hasPred : HasPred[D#Pred] = ev

          val theSources : Vector[Cell[D#Pred#Pred, CellType]] = srcs.flatten.cells

          // I don't like this.  We should use the permutation ...
          tgtMutableCell.canopy = Some(CellTree.toRoseTree(srcs, ev).map((c => { c.value.container = Some(tgtMutableCell) ; c.value }), 
                                                                         (l => theSources.indexOf(l))))
          tgtMutableCell.target = Some(srcs.output.value)
          tgtMutableCell.sources = Some(theSources map (_.value))
        }
      }

      if (tgtMutableCell.isLoop) {
        for { t <- tgtMutableCell.target } {
          t.loops = tgtMutableCell :: t.loops
        }
      }

      val thisMutableCellSkeleton = CompositeCell(thisMutableCell, srcs, tgtMutableCell)
      thisMutableCell.skeleton = thisMutableCellSkeleton
      tgtMutableCell.skeleton = thisMutableCellSkeleton.target

      thisMutableCellSkeleton
    }
  }
}
