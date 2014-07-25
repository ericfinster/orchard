/**
  * Framework.scala - Frameworks
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.checker

import orchard.core.cell._
import orchard.core.util._
import orchard.core.complex._

import ErrorM._

abstract class Framework[A : ExpressionLike](seed : NCell[A])
    extends MutableSkeletalComplex[A] { thisFramework =>

  type FrameworkType <: Framework[A]
  type CellType <: FrameworkCell

  def extract(cell : CellType) : FrameworkType
  def duplicate : FrameworkType = extract(topCell)

  def stabilityLevel : Option[Int]
  def invertibilityLevel : Option[Int]
  def unicityLevel : Option[Int]

  trait FrameworkCell extends MutableSkeletalCell { thisCell : CellType =>

    var canopy : Option[RoseTree[CellType, Int]] = None
    var target : Option[CellType] = None 
    var sources : Option[Vector[CellType]] = None
    var container : Option[CellType] = None

    var incoming : Option[CellType] = None
    var outgoing : Option[CellType] = None

    // Umm ....
    var skeleton : NCell[CellType] = null

    def isThin : Boolean =
      implicitly[ExpressionLike[A]].isThin(item)

    def isEmpty : Boolean =
      implicitly[ExpressionLike[A]].isEmpty(item)

    def isFull : Boolean = ! isEmpty

    def emptySources : Vector[CellType] =
      sources match {
        case None => Vector.empty
        case Some(srcs) => srcs filter (_.isEmpty)
      }

    def fullSources : Vector[CellType] =
      sources match {
        case None => Vector.empty
        case Some(srcs) => srcs filter (_.isFull)
      }

    def fullFaces : Vector[CellType] =
      target match {
        case None => Vector.empty
        case Some(tgt) => if (tgt.isEmpty) fullSources else (tgt +: fullSources)
      }

    // For an exposed nook, get the cell corresponding to the boundary
    def boundaryFace : CellType =
      if (target.get.isEmpty)
        target.get
      else
        emptySources.head

    def boundaryAddress : CellAddress =
      boundaryFace.address

    def completeSources : Vector[CellType] =
      sources match {
        case None => Vector.empty
        case Some(srcs) => srcs filter (_.isComplete)
      }

    def isInNook : Boolean =
      target match {
        case None => false
        case Some(tgt) => {
          sources match {
            case None => false
            case Some(srcs) => {
              isEmpty && tgt.isComplete && (emptySources.length == 1) &&
              (1 + completeSources.length == sourceCount)
            }
          }
        }
      }

    def isOutNook : Boolean =
      target match {
        case None => false
        case Some(tgt) => {
          if (sourceCount == 0) {
            return (isEmpty && tgt.isShell)
          }

          sources match {
            case None => isEmpty && tgt.isShell
            case Some(srcs) => {
              isEmpty && tgt.isEmpty && (true /: (srcs map (_.isComplete))) (_ && _)
            }
          }
        }
      }

    def isNook : Boolean = isInNook || isOutNook

    // Okay, I'm pretty sure this is wildly inefficient and could be done much faster. Please
    // have a look at it again before deleting this comment. :)
    def isExposedNook : Boolean = {
      if (isOutNook) true else {
        if (isInNook) {
          val framework = extract(thisCell)
          val frameworkTgt = framework.topCell.target.get

          val emptyPtr = (new RoseZipper(frameworkTgt.canopy.get, Nil)).find(c => c.isEmpty).get

          var status : Boolean = true

          def getDerivedOutNook(cell : framework.CellType) : Framework[A] = {
            val derivedFramework = framework.extract(cell)
            derivedFramework.topCell.item = implicitly[ExpressionLike[A]].empty
            derivedFramework.topCell.target.get.item = implicitly[ExpressionLike[A]].empty
            derivedFramework
          }

          def checkBranchList(branches : Vector[RoseTree[framework.CellType, Int]]) = {
            branches foreach
            (branch => {
              branch foreachCell
              (cell => {
                status &&= getDerivedOutNook(cell).topCell.isExposedNook
              })
            })
          }

          // So, now we've got the right tree.
          emptyPtr.focus match {
            case Branch(emptyCell, branches) => checkBranchList(branches)
            case _ => throw new IllegalArgumentException("Ummm ...")
          }

          // First step is done ...
          if (! status) return false

          var ptr : RoseZipper[framework.CellType, Int] = emptyPtr

          while (ptr.context != Nil && status) {
            ptr = ptr.context match {
              case RoseContext(value, left, right) :: cs => {

                val incomingFace = ptr.focus.rootElement.get.target.get

                val faceSave = incomingFace.item
                val sourceSave = value.item

                incomingFace.item = implicitly[ExpressionLike[A]].empty
                value.item = implicitly[ExpressionLike[A]].empty

                val derivedNook = framework.extract(value)
                status &&= derivedNook.topCell.isExposedNook

                incomingFace.item = faceSave
                value.item = sourceSave

                // Now work on the left and right lists ...
                checkBranchList(left)
                checkBranchList(right)

                RoseZipper(Branch(value, left ++ List(ptr.focus) ++ right), cs)
              }
              case _ => throw new IllegalArgumentException
            }
          }

          status && (true /: (fullSources map (_.isThin))) (_ && _)
        } else { false }
      }
    }

    def hasCompleteShell : Boolean = {
      var result = true
      skeleton map (face => if (face != thisCell) { result &&= face.isFull } )
      result
    }

    def isShell : Boolean = isEmpty && hasCompleteShell
    def isComplete : Boolean = isFull && hasCompleteShell

    def isUnicityFillable : Boolean =
      unicityLevel match {
        case None => false
        case Some(l) => isShell && (dimension > l)
      }

    def isFillable : Boolean =
      if (isUnicityFillable) true else isExposedNook

    // For an exposed nook, determine if the filler face is thin
    def isThinBoundary : Boolean = {
      val thinByInvertibility =
        invertibilityLevel match {
          case None => false
          case Some(l) => (dimension - 1) > l
        }

      if (isOutNook) {
        (true /: (sources.get map (_.isThin))) (_&&_) || thinByInvertibility
      } else {
        target.get.isThin || thinByInvertibility
      }
    }
  }
}
