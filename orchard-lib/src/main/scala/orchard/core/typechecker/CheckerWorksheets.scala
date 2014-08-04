/**
  * CheckerWorksheets.scala - Worksheets
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.typechecker

import orchard.core.cell._
import orchard.core.util._
import orchard.core.complex._

import ErrorM._

trait CheckerWorksheets { thisChecker : Checker =>

  sealed trait WorksheetEntry extends FrameworkEntry

  case class Neutral(entry : SimpleEntry) extends WorksheetEntry {
    def isEmpty = entry.isEmpty
    def isThin = entry.isThin
    def expression = entry.expression
  }

  case object Positive extends WorksheetEntry {
    def isEmpty = true
    def isThin = checkerFail("Thin request on positive cell")
    def expression = throw new Exception("Positive cell has no expression")
  }

  case object Negative extends WorksheetEntry {
    def isEmpty = true
    def isThin = checkerFail("Thin request on negative cell")
    def expression = throw new Exception("Negative cell has no expression")
  }

  class Worksheet(seed : NCell[WorksheetEntry]) 
      extends AbstractComplex[WorksheetEntry](seed) 
      with Framework[WorksheetEntry] 
      with SelectableComplex[WorksheetEntry] {

    type CellType = WorksheetCell
    type FrameworkType = Worksheet

    def newCell(item : WorksheetEntry) = new WorksheetCell(item)
    def extract(cell : WorksheetCell) = new Worksheet(cell.skeleton map (_.item))

    def emptyItem = Neutral(Empty)

    class WorksheetCell(var item : WorksheetEntry) extends AbstractComplexCell with FrameworkCell {

      def isPolarized : Boolean = 
        item match {
          case Positive => true
          case Negative => true
          case Neutral(_) => false
        }

    }

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

    def emptyExtrusion : Error[Unit] =
      extrudeAtSelection(Empty, Empty)

    def extrudeAtSelection(targetExpr : SimpleEntry, fillerExpr : SimpleEntry) : Error[Unit] = {
      selectionBase match {
        case None => fail("Nothing selected.")
        case Some(base) => {

          // Make sure we have enough space
          if (base.dimension >= dimension - 1)
            extend

          val baseContainer = base.container.get
          val basePtr = (new RoseZipper(baseContainer.canopy.get, Nil))
            .lookup(base).get

          val (targetCell, fillerCell) =
            baseContainer.insertComposite(Neutral(targetExpr), Neutral(fillerExpr), basePtr, (cell => selectedCells contains cell))

          clearAndSelect(targetCell)

          success(())
        }
      }
    }

    def emptyDrop : Error[Unit] =
      dropAtSelection(Empty, Empty)

    def dropAtSelection(compositeExpr : SimpleEntry, fillerExpr : SimpleEntry) : Error[Unit] =
      selectionBase match {
        case None => fail("Nothing selected.") //CheckerFailure("Nothing selected")
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

          success(())
        }
      }

  }


}
