/**
  * Expression.scala - Simple opetopic expressions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

import scala.collection.mutable.ListBuffer

import Util._

sealed trait Expression { def id : String ; def isThin : Boolean }

case class Variable(val id : String, val isThin : Boolean) extends Expression { 
  override def toString = id 
}

case class Filler(val id : String, val nook : NCell[Option[Expression]]) extends Expression { 
  def isThin = true 
  override def toString = id 
}

case class FillerTarget(val id : String, val nook : NCell[Option[Expression]], val isThin : Boolean) extends Expression { 
  override def toString = id
}

class ExpressionComplex(seed : NCell[Expression]) extends CellComplex[Expression] {

  type CellType = ExpressionCell

  //============================================================================================
  // COMPLEX IMPLEMENTATION
  //

  def newCell(expr : Expression) = new ExpressionCell(expr)

  populateComplex(seed)

  //============================================================================================
  // CELL IMPLEMENTATION
  //

  class ExpressionCell(val item : Expression) extends ComplexCell {

    def isThin = item.isThin

  }

}

// Oh.  We can just use the framework.
class SimpleFramework(seed : NCell[Option[Expression]]) 
    extends MutableComplex[Option[Expression]] with ExpressionFramework[Option[Expression]] {

  type CellType = SimpleFrameworkCell

  def newCell(item : Option[Expression]) = new SimpleFrameworkCell(item)

  populateComplex(seed)

  class SimpleFrameworkCell(var item : Option[Expression])
      extends MutableCell with ExpressionFrameworkCell {

    def exprItem = item

  }

}

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

    def emptySources : List[CellType] =
      sources match {
        case None => Nil
        case Some(srcs) => srcs filter (_.isEmpty)
      }

    def fullSources : List[CellType] =
      sources match {
        case None => Nil
        case Some(srcs) => srcs filter (_.isFull)
      }

    def completeSources : List[CellType] =
      sources match {
        case None => Nil
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
          sources match {
            case None => isEmpty && tgt.isEmpty
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

          val framework : SimpleFramework = 
            if (thisFramework.isInstanceOf[SimpleFramework]) {
              if (isTopCell) thisFramework.asInstanceOf[SimpleFramework] else getSimpleFramework
            } else getSimpleFramework

          val frameworkTgt = framework.topCell.target.force
          val emptyPtr = (new RoseZipper(frameworkTgt.shell.force, Nil)).find(c => c.item == None).force

          var status : Boolean = true

          def getDerivedOutNook(cell : framework.CellType) : SimpleFramework = {
            val derivedFramework = cell.getSimpleFramework
            derivedFramework.topCell.item = None
            derivedFramework.topCell.target.force.item = None
            derivedFramework
          }

          def checkBranchList(branches : List[RoseTree[framework.CellType, Int]]) = {
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
        } else false
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

class ExpressionBuilderComplex(seed : NCell[Polarity[Option[Expression]]])
    extends MutableComplex[Polarity[Option[Expression]]]
    with CardinalComplex[Option[Expression]] 
    with ExpressionFramework[Polarity[Option[Expression]]] {

  type CellType = ExpressionBuilderCell

  // Initialize from the seed ...
  populateComplex(seed)

  def newCell(expr : Polarity[Option[Expression]]) = new ExpressionBuilderCell(expr)
  def extend = glob(Negative, Positive)

  class ExpressionBuilderCell(initialItem : Polarity[Option[Expression]]) 
      extends MutableCell 
      with ExpressionFrameworkCell
      with CardinalCell { thisCell =>

    private var myItem : Polarity[Option[Expression]] = initialItem

    def item = myItem
    def item_=(newItem : Polarity[Option[Expression]]) = {
      val oldItem = myItem
      myItem = newItem
      emit(ChangeEvents.ItemChangedEvent(oldItem))
    }

    def exprItem = 
      item match {
        case Neutral(expr) => expr
        case _ => throw new IllegalArgumentException("Expression error")
      }
  }

}
