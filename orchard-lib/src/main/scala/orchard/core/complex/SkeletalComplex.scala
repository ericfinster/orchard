/**
  * SkeletalComplex.scala - Base trait for complexes which include their skeleton
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.complex

import orchard.core.cell._
import orchard.core.util._

trait SkeletalComplex[A] extends CellComplex[A] {

  type CellType <: SkeletalCell

  trait SkeletalCell extends ComplexCell { 
    thisCell : CellType =>

    var skeleton : NCell[CellType]
    def toNCell : NCell[A] = skeleton map (_.item)

  }

  //============================================================================================
  // COMPLEX GENERATION
  //

  object ComplexGenerator extends CellRegenerator[A, CellType] {

    def generateObject[D <: Nat : IsZero](value : A) : Cell[D, CellType] = {
      val newObj = newCell(value)
      val newObjSkeleton = ObjectCell(newObj)
      newObj.skeleton = newObjSkeleton
      newObjSkeleton
    }

    def generateCell[D <: Nat : HasPred](cellValue : A,
                                         srcs : CellTree[D#Pred, CellType],
                                         tgtValue : A) : Cell[D, CellType] = {

      val thisCell = newCell(cellValue)
      val tgtCell = newCell(tgtValue)

      thisCell.canopy = None
      thisCell.target = Some(tgtCell)
      thisCell.sources = Some(srcs.cells map
                                       (src => {
                                          src.value.outgoing = Some(thisCell)
                                          src.value
                                        }))

      // We know the incoming cells
      thisCell.incoming = None
      tgtCell.incoming = Some(thisCell)

      var curIdx : Int = -1
      val perm = srcs.inversePerm

      // Yeah, um, this is not good.  What you do is simply flatten the leaves
      // into a list.  But they should be put in the correct order based on flattening.

      srcs.dimension match {
        case IsZero(ev) => {
          implicit val isZero : IsZero[D#Pred] = ev

          val theObject = srcs.cells(0).value
          theObject.container = Some(tgtCell)

          tgtCell.canopy = Some(Branch(srcs.cells(0).value, Vector(Rose(0))))
          tgtCell.target = None
          tgtCell.sources = None

        }
        case HasPred(ev) => {
          implicit val hasPred : HasPred[D#Pred] = ev

          val theSources : Vector[Cell[D#Pred#Pred, CellType]] = srcs.flatten.cells

          // I don't like this.  We should use the permutation ...
          tgtCell.canopy = Some(CellTree.toRoseTree(srcs, ev).map((c => { c.value.container = Some(tgtCell) ; c.value }), 
                                                                         (l => theSources.indexOf(l))))
          tgtCell.target = Some(srcs.output.value)
          tgtCell.sources = Some(theSources map (_.value))
        }
      }

      if (tgtCell.isLoop) {
        for { t <- tgtCell.target } {
          t.loops = tgtCell :: t.loops
        }
      }

      val thisCellSkeleton = CompositeCell(thisCell, srcs, tgtCell)
      thisCell.skeleton = thisCellSkeleton
      tgtCell.skeleton = thisCellSkeleton.target

      thisCellSkeleton
    }
  }
}
