/**
  * Workspace.scala - Trait encapsulating the worksheet view of expression manipulation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

import orchard.core.cell._
import orchard.core.complex._

import IdentParser.Success
import IdentParser.NoSuccess

trait WorkspaceModule { thisModule : InteractiveTypeChecker =>

  abstract class Workspace(val module : ModuleType) { thisWorkspace =>

    def getActiveWorksheet : CheckerResult[Worksheet]
    def setActiveWorksheet(sheet : Worksheet) : CheckerResult[Unit]

    def activeWorksheet : Option[Worksheet]

    def stabilityLevel : Option[Int]
    def invertibilityLevel : Option[Int]
    def unicityLevel : Option[Int]

    //============================================================================================
    // SEMANTIC ROUTINES
    //

    def assumeAtSelection(thinHint : Boolean) : CheckerResult[Unit] =
      for {
        worksheet <- getActiveWorksheet
        selectedCell <- worksheet.getSelectionBase
        _ <- verify(worksheet.selectionIsUnique, "Selection is not unique")
        _ <- verify(worksheet.selectionIsShell, "Selection is not a shell")
      } yield {
        
        val shell = new Shell(new WorkspaceFramework(selectedCell.neutralNCell))

        val forceThin =
          invertibilityLevel match {
            case None => false
            case Some(l) => selectedCell.dimension > l
          }

        // What you want here is scala's Future monad.  But I don't really know how to
        // use the execution contexts correctly in javafx, so this will have to wait 
        // just a bit ...
        editor.withAssumptionInfo(thinHint, forceThin,
          (identString, isThin) => {
            for {
              param <- appendParameter(module, identString, shell, isThin)
            } yield {

              val varRef = Reference(param)

              worksheet.deselectAll
              selectedCell.item = Neutral(Some(varRef))
              worksheet.selectAsBase(selectedCell)

              CheckerResult(())
            }
          }
        )
      }

    def fillAtSelection : CheckerResult[Unit] =
      for {
        worksheet <- getActiveWorksheet
        selectedCell <- worksheet.getSelectionBase
        _ <- verify(worksheet.selectionIsExposedNook, "Selection is not an exposed nook")
      } yield {

        val nook = new Nook(new WorkspaceFramework(selectedCell.neutralNCell))

        // Oops.  This guy should resolve to a checker command, which the handler
        // then executes ...
        editor.withFillerIdentifier(
          identString => {
            for {
              lift <- appendLift(module, identString, nook)
            } yield {

              val fillerRef = Reference(lift.fillerEntry)
              val bdryRef = Reference(lift)

              val boundaryCell = selectedCell.boundaryFace

              worksheet.deselectAll
              selectedCell.item = Neutral(Some(fillerRef))
              boundaryCell.item = Neutral(Some(bdryRef))
              worksheet.selectAsBase(selectedCell)

              CheckerResult(())
            }
          })

      }
    
    // !!! BUG !!! - Remember you need to keep track of some kind of bindings
    // because this will allow for f : x -> y to be pasted into a loop
    def pasteToSelection(pasteExpr : Expression) : CheckerResult[Unit] = 
      for {
        worksheet <- getActiveWorksheet
        selectedCell <- worksheet.getSelectionBase
        _ <- verify(worksheet.selectionIsUnique, "Selection is not unique")
        zippedTree <- zipShapes(selectedCell.skeleton, pasteExpr.ncell)
        okToPaste <- shapeSequence(
          zippedTree map {
            case (cell, expr) =>
              if (cell.isEmpty) CheckerResult(true) else {
                cell.item match {
                  case Neutral(Some(tgtExpr)) => 
                    if (convertsTo(tgtExpr, expr))
                      CheckerResult(true)
                    else
                      CheckerFailure("Expression " ++ tgtExpr.name ++ " does not convert to " ++ expr.name)
                  case _ => CheckerFailure("Unexpected value encountered ...")
                }
              }
          }
        )
      } yield {
        worksheet.deselectAll

        zippedTree map {
          case (cell, expr) => {
            if (cell.isEmpty) {
              cell.item = Neutral(Some(expr))
            }
          }
        }
      }

    // def importActiveInstantiation : Unit =
    //   for {
    //     instntr <- activeInstantiator
    //   } {
    //     if (instntr.isComplete) {
    //       appendInstantiation(instntr.shell, ExternalReference(instntr.lift), instntr.bindings)
    //       //newSheet(instntr.completedExpression)
    //     } else {
    //       editor.consoleError("There are unbound variables.")
    //     }
    //   }


    //============================================================================================
    // WORKSHEETS
    //

    class Worksheet(seed : NCell[Polarity[Option[Expression]]])
        extends AbstractWorksheet(seed) {

      type FrameworkType = Worksheet
      type CellType = WorksheetCell

      def stabilityLevel : Option[Int] = thisWorkspace.stabilityLevel
      def invertibilityLevel : Option[Int] = thisWorkspace.invertibilityLevel
      def unicityLevel : Option[Int] = thisWorkspace.unicityLevel

      def newCell(item : Polarity[Option[Expression]]) =
        new WorksheetCell(item)

      def extract(cell : CellType) =
        new Worksheet(cell.skeleton map (_.item))

      class WorksheetCell(itm : Polarity[Option[Expression]])
          extends AbstractWorksheetCell(itm) {

      }
    }

    //============================================================================================
    // FRAMEWORKS
    //

    class WorkspaceFramework(seed : NCell[Option[Expression]])
        extends Framework(seed) {

      def this(expr : Expression) = this(expr.ncell map (Some(_)))

      type FrameworkType = WorkspaceFramework
      type CellType = WorkspaceFrameworkCell

      def stabilityLevel : Option[Int] = thisWorkspace.stabilityLevel
      def invertibilityLevel : Option[Int] = thisWorkspace.invertibilityLevel
      def unicityLevel : Option[Int] = thisWorkspace.unicityLevel

      def newCell(item : Option[Expression]) =
        new WorkspaceFrameworkCell(item)

      def extract(cell : CellType) =
        new WorkspaceFramework(cell.skeleton map (_.item))

      class WorkspaceFrameworkCell(var item : Option[Expression])
          extends FrameworkCell {

        def expression : Option[Expression] = item

        override def toString = item.toString

      }

    }

  }

}
