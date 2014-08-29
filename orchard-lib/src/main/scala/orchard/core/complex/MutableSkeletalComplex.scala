/**
  * MutableSkeletalComplex.scala - Mutability routines for a complex with a skeleton
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.complex

import orchard.core.util._
import orchard.core.cell._

import ErrorM._

trait MutableSkeletalComplex[A] extends MutableComplex[A] with SkeletalComplex[A] { 
  thisComplex =>

  type CellType <: MutableSkeletalCell

  override def glob(globValue : A, targetValue : A) : CellType = {
    val globCell = super.glob(globValue, targetValue)
    val sourceCell = globCell.sources.get.head
    val targetCell = globCell.target.get

    val sourceSkeleton = sourceCell.skeleton
    val glob : Cell[S[sourceSkeleton.dim.Self], CellType] = 
      sourceSkeleton.glob(globCell, targetCell)

    globCell.skeleton = glob
    targetCell.skeleton = glob.target

    globCell
  }

  trait MutableSkeletalCell extends MutableCell with SkeletalCell { thisCell : CellType =>

    override def insertComposite(
      compositeValue : A, universalValue : A,
      location : RoseZipper[CellType, Int],
      selector : CellType => Boolean
    ) : (CellType, CellType) = {
      val (compositeCell, universalCell) = 
        super.insertComposite(
          compositeValue, universalValue,
          location, selector
        )

      // Fix all the skeletons in higher dimensions
      for {
        d <- Range(dimension, thisComplex.dimension + 1)
      } {
        thisComplex(d).rigidify
      }

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

          val mySrcTree : RoseTree[CellType, Int] = myTarget.extractCanopy(myExtendedSources)
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
  }
}
