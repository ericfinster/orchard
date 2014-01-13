/**
  * ExpressionFramework.scala - Trait for a cell complex which represents and expression
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

import Util._

trait ExpressionFramework[A] extends CellComplex[A] { thisFramework =>

  type CellType <: ExpressionFrameworkCell

  trait ExpressionFrameworkCell extends ComplexCell { thisCell : CellType =>

    def exprItem : Option[Expression]

    def isThin =
      exprItem match {
        case Some(expr) => expr.isThin
        case _ => false
      }

    def isEmpty : Boolean = exprItem == None
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
          println("It's an inNook")

          val framework : SimpleFramework = 
            if (thisFramework.isInstanceOf[SimpleFramework]) {
              if (isTopCell) thisFramework.asInstanceOf[SimpleFramework] else getSimpleFramework
            } else getSimpleFramework

          val frameworkTgt = framework.topCell.target.force

          // if (framework.topCell.isDrop) {
          //   println("Testing a drop")
          //   return frameworkTgt.isShell
          // }

          val emptyPtr = (new RoseZipper(frameworkTgt.canopy.force, Nil)).find(c => c.item == None).force

          var status : Boolean = true

          def getDerivedOutNook(cell : framework.CellType) : SimpleFramework = {
            val derivedFramework = cell.getSimpleFramework
            derivedFramework.topCell.item = None
            derivedFramework.topCell.target.force.item = None
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

                incomingFace.item = None
                value.item = None

                val derivedNook = value.getSimpleFramework
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

    def getSimpleFramework : SimpleFramework = {
      new SimpleFramework(skeleton map (cell => cell.exprItem))
    }
  }
}
