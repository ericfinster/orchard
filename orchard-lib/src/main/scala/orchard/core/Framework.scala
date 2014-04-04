/**
  * Framework.scala - An abstract class for complexes that may act as frameworks
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

import scala.language.implicitConversions
import scala.collection.mutable.Buffer

import Util._

abstract class Framework[A : HasEmpty](seed : NCell[A]) extends AbstractMutableComplex[A](seed) { thisFramework =>

  type CellType <: FrameworkCell

  type ExprIndexType
  type ExprType = Expression[ExprIndexType]

  def extract(cell : CellType) : Framework[A]
  def getExpression(a : A) : Option[ExprType]

  abstract class FrameworkCell(itm : A) extends AbstractMutableCell { thisCell : CellType =>

    protected var frameworkItem = itm
    protected var expressionItem = getExpression(itm)

    def item = frameworkItem
    def item_=(newItm : A) = {
      val oldItem = frameworkItem
      frameworkItem = newItm
      expressionItem = getExpression(newItm)
      emit(ChangeEvents.ItemChangedEvent(oldItem))
    }

    def expression : Option[ExprType] = expressionItem

    def isThin : Boolean =
      expression match {
        case None => false
        case Some(expr) => expr.isThin
      }

    def isEmpty : Boolean = expression == None
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
            derivedFramework.topCell.item = implicitly[HasEmpty[A]].empty
            derivedFramework.topCell.target.force.item = implicitly[HasEmpty[A]].empty
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

                incomingFace.item = implicitly[HasEmpty[A]].empty
                value.item = implicitly[HasEmpty[A]].empty

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

  }
}

