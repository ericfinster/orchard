/**
  * AbstractWorksheet.scala - An abstract version of a worksheet
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

import orchard.core.cell._
import orchard.core.util._
import orchard.core.complex._

abstract class AbstractWorksheet(seed : NCell[Polarity[Option[Expression]]])
    extends Framework[Polarity[Option[Expression]]](seed)
    with SelectableComplex[Polarity[Option[Expression]]]
    with CardinalComplex[Option[Expression]] {

  type CellType <: AbstractWorksheetCell

  abstract class AbstractWorksheetCell(itm : Polarity[Option[Expression]])
      extends AbstractMutableCell
      with FrameworkCell
      with CardinalCell { thisCell : CellType => 

      protected var myItem = itm

      def item = myItem
      def item_=(newItm : Polarity[Option[Expression]]) = {
        val oldItem = item
        myItem = newItm
        emit(ChangeEvents.ItemChangedEvent(oldItem))
      }
  }

  def extend = glob(Negative, Positive)

  def selectionIsComposable : Boolean = {
    val cellsAreComplete = (true /: (selectedCells map (_.isComplete))) (_&&_)
    cellsAreComplete && selectionIsExtrudable
  }

  def selectionIsShell : Boolean = {
    if (! selectionIsUnique) false else
      selectionBase match {
        case None => false
        case Some(cell) => cell.isShell
      }
  }

  def selectionIsEmptyCell : Boolean = {
    if (! selectionIsUnique) false else
      selectionBase match {
        case None => false
        case Some(cell) => cell.isEmpty
      }
  }

  def selectionIsExtrudable : Boolean = {
    selectionBase match {
      case None => false
      case Some(cell) => {
        cell.container match {
          case None => false
          case Some(cont) => cont.isPolarized
        }
      }
    }
  }

  def selectionIsDroppable : Boolean = {
    selectionBase match {
      case None => false
      case Some(cell) => {
        cell.outgoing match {
          case None => false  // This shouldn't happen
          case Some(o) => {
            if (o.isPolarized) true else {
              o.container match {
                case None => false
                case Some(cont) => cont.isPolarized
              }
            }
          }
        }
      }
    }
  }

  def extrude = {
    if (selectionIsExtrudable) {
      emptyExtrusion
    } else {
      println("Selection is not extrudable.")
    }
  }

  def drop = {
    if (selectionIsDroppable) {
      emptyDrop
    } else {
      println("Selection is not droppable.")
    }
  }

  def emptyExtrusion = extrudeAtSelection(None, None)

  def extrudeAtSelection(targetExpr : Option[Expression], fillerExpr : Option[Expression]) =
    selectionBase match {
      case None => ()
      case Some(base) => {

        // Make sure we have enough space
        if (base.dimension >= dimension - 1)
          extend

        // I think the casting is unnecessary: you know that you're looking
        // for the positive container, which you can get by hand ... see below
        val baseContainer = base.container.get

        val basePtr = (new RoseZipper(baseContainer.canopy.get, Nil))
          .lookup(base).get

        val (targetCell, fillerCell) =
          baseContainer.insertComposite(Neutral(targetExpr), Neutral(fillerExpr), basePtr, (cell => selectedCells contains cell))

        clearAndSelect(targetCell)
      }
    }

  def emptyDrop = dropAtSelection(None, None)

  def dropAtSelection(compositeExpr : Option[Expression], fillerExpr : Option[Expression]) =
    selectionBase match {
      case None => ()
      case Some(base) => {
        if (base.dimension == dimension - 2) {
          extend
        } else if (base.dimension == dimension - 1) {
          extend ; extend
        }

        val positiveBase = this(base.dimension + 1)

        val outPtr =
          new RoseZipper(positiveBase.canopy.get, Nil).
            lookup(base.outgoing.get).get

        val basePtr = outPtr.focus match {
          case Rose(_) => throw new IllegalArgumentException("Didn't find the outgoing cell!")
          case Branch(outCell, brs) => {
            val i = brs indexWhere
            (branch =>
              branch match {
                case Rose(idx) => {
                  val srcs = positiveBase.sources.get
                  if (srcs(idx) == base) true else false
                }
                case Branch(cell, _) => {
                  if (cell.target.get == base) true else false
                }
              })

            outPtr.visitBranch(i).get
          }
        }

        val (targetCell, fillerCell) =
          positiveBase.insertComposite(Neutral(compositeExpr), Neutral(fillerExpr), basePtr, (_ => false))

        clearAndSelect(targetCell)
      }
    }
}

