/**
  * Framework.scala - An abstract class for complexes that may act as frameworks
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

import orchard.core.cell._
import orchard.core.util._
import orchard.core.complex._

import Util._

trait ExpressionLike[A] {

  def empty : A

  def isThin(a : A) : Boolean
  def isEmpty(a : A) : Boolean

}

object ExpressionLike {

  implicit def markerIsExpressionLike : ExpressionLike[ExpressionMarker] =
    new ExpressionLike[ExpressionMarker] {

      def empty : ExpressionMarker = Empty

      def isEmpty(mkr : ExpressionMarker) = mkr == Empty
      def isThin(mkr : ExpressionMarker) =
        mkr match {
          case Empty => false
          case m : Marker => m.isThin
        }
    }

}

abstract class Framework[A : ExpressionLike] extends MutableComplex[A] { thisFramework =>

  type FrameworkType <: Framework[A]
  type CellType <: FrameworkCell

  def extract(cell : CellType) : FrameworkType
  def duplicate : FrameworkType = extract(topCell)
  def newFromExpression(expr : Expression) : FrameworkType

  def stabilityLevel : Option[Int]
  def invertibilityLevel : Option[Int]
  def unicityLevel : Option[Int]

  trait FrameworkCell extends MutableCell { thisCell : CellType =>

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
          val frameworkTgt = framework.topCell.target.force

          val emptyPtr = (new RoseZipper(frameworkTgt.canopy.force, Nil)).find(c => c.isEmpty).force

          var status : Boolean = true

          def getDerivedOutNook(cell : framework.CellType) : Framework[A] = {
            val derivedFramework = framework.extract(cell)
            derivedFramework.topCell.item = implicitly[ExpressionLike[A]].empty
            derivedFramework.topCell.target.force.item = implicitly[ExpressionLike[A]].empty
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

                val incomingFace = ptr.focus.rootElement.force.target.force

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

