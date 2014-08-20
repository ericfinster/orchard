/**
  * Worksheets.scala - Worksheets
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.free

import orchard.core.cell._
import orchard.core.util._
import orchard.core.complex._

import ErrorM.{succeed => succeedE, fail => failE, _}

trait Worksheets { thisChecker : TypeChecker =>

  type WorksheetEntry = Polarity[FrameworkEntry]

  import ExpressionContainer._

  implicit val worksheetEntryIsContainer : ExpressionContainer[WorksheetEntry] = 
    new ExpressionContainer[WorksheetEntry] {

      def empty : WorksheetEntry = Neutral(Empty)

      def expression(entry : WorksheetEntry) : Error[Expression] = 
        entry match {
          case Positive => failE("Positive cell contains no expression")
          case Negative => failE("Negative cell contains no expression")
          case Neutral(frmwkEntry) => frmwkEntry.expression
        }

    }

  class Worksheet(seed : NCell[WorksheetEntry])
      extends AbstractFramework[WorksheetEntry](seed)
      with CardinalComplex[FrameworkEntry]
      with SelectableComplex[WorksheetEntry] {

    type CellType = WorksheetCell
    type FrameworkType = Worksheet

    def newCell(item : WorksheetEntry) = new WorksheetCell(item)
    def extract(cell : WorksheetCell) = new Worksheet(cell.skeleton map (_.item))

    class WorksheetCell(var item : WorksheetEntry) extends AbstractFrameworkCell with CardinalCell

    def extend = glob(Negative, Positive)

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

    def emptyExtrusion : Error[(CellAddress, CellAddress)] =
      extrudeAtSelection(Empty, Empty)

    def extrudeAtSelection(targetExpr : FrameworkEntry, fillerExpr : FrameworkEntry) : Error[(CellAddress, CellAddress)] = {
      selectionBase match {
        case None => failE("Nothing selected.")
        case Some(base) => {

          // Make sure we have enough space
          if (base.dimension >= dimension - 1)
            extend

          val baseContainer = base.container.get
          val basePtr = (new RoseZipper(baseContainer.canopy.get, Nil))
            .lookup(base).get

          val (targetCell, fillerCell) =
            baseContainer.insertComposite(Neutral(targetExpr), Neutral(fillerExpr), basePtr, (cell => selectedCells contains cell))

          succeedE((fillerCell.address, targetCell.address))
        }
      }
    }

    def emptyDrop : Error[(CellAddress, CellAddress)] =
      dropAtSelection(Empty, Empty)

    def dropAtSelection(compositeExpr : FrameworkEntry, fillerExpr : FrameworkEntry) : Error[(CellAddress, CellAddress)] =
      selectionBase match {
        case None => failE("Nothing selected.") //CheckerFailure("Nothing selected")
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

          succeedE((fillerCell.address, targetCell.address))
        }
      }

  }

  object Worksheet {

    def apply(seed : NCell[FrameworkEntry]) : Worksheet =
      new Worksheet(CardinalComplex(seed))

  }

}
