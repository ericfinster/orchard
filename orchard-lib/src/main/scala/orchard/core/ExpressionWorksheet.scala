/**
  * ExpressionWorksheet.scala - A complex which holds incomplete expressions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

import Util._

class ExpressionWorksheet(seed : NCell[Polarity[Option[Expression]]])
    extends AbstractMutableComplex[Polarity[Option[Expression]]](seed)
    with CardinalComplex[Option[Expression]] 
    with SelectableComplex[Polarity[Option[Expression]]]
    with ExpressionFramework[Polarity[Option[Expression]]] {

  def this() = this(Composite(Negative, Seed(Object(Neutral(None))), Positive))

  type CellType = ExpressionWorksheetCell

  def newCell(expr : Polarity[Option[Expression]]) = new ExpressionWorksheetCell(expr)

  def extend = glob(Negative, Positive)

  class ExpressionWorksheetCell(initialItem : Polarity[Option[Expression]]) 
      extends AbstractMutableCell
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

    override def toString = "ExprCell(" ++ item.toString ++ ")@" ++ hashCode.toString
  }

  //============================================================================================
  // SEMANTICS
  //

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

  def extrudeSelection = {
    if (selectionIsExtrudable) {
      emptyComposition
    } else {
      println("Selection is not extrudable.")
    }
  }

  def extrudeDrop = {
    if (selectionIsDroppable) {
      emptyDrop
    } else {
      println("Selection is not droppable.")
    }
  }

  def emptyComposition = composeSelection(None, None)

  def composeSelection(compositeExpr : Option[Expression], fillerExpr : Option[Expression]) = 
    selectionBase match {
      case None => ()
      case Some(base) => {

        // Make sure we have enough space
        if (base.dimension >= dimension - 1)
          extend

        // I think the casting is unnecessary: you know that you're looking
        // for the positive container, which you can get by hand ... see below
        val baseContainer = base.container.get

        val basePtr = (new RoseZipper(baseContainer.canopy.force, Nil))
          .lookup(base).force("Lookup failed for selection base")

        val (targetCell, fillerCell) = 
          baseContainer.insertComposite(Neutral(compositeExpr), Neutral(fillerExpr), basePtr, (cell => selectedCells contains cell))

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
            lookup(base.outgoing.get).force("Lookup failed somehow.")

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
