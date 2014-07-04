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
            } {

              val varRef = Reference(param.name, VariableType, param.isThin)

              worksheet.deselectAll
              selectedCell.item = Neutral(Some(varRef))
              worksheet.selectAsBase(selectedCell)

            }
          }
        )
      }

    def fillAtSelection =
      for {
        worksheet <- activeWorksheet
        selectedCell <- worksheet.selectionBase
      } {
        if (selectedCell.isUnicityFillable) {

          ???

        } else if (selectedCell.isExposedNook) {

          val nook = new Nook(new WorkspaceFramework(selectedCell.neutralNCell))

          editor.withFillerIdentifier(
            identString => {
              for {
                lift <- appendLift(module, identString, nook)
              } {

                val fillerRef = Reference(lift.name, FillerType, true)
                val bdryRef = Reference(lift.name, BoundaryType, lift.filler.Boundary.isThin)

                val boundaryCell = selectedCell.boundaryFace

                worksheet.deselectAll
                selectedCell.item = Neutral(Some(fillerRef))
                boundaryCell.item = Neutral(Some(bdryRef))
                worksheet.selectAsBase(selectedCell)

              }
            })
        } else {
          editor.consoleError("Selection is not fillable.")
        }
      }
    
    // def pasteToSelection(pasteExpr : Expression) =
    //   for {
    //     worksheet <- activeWorksheet
    //     if (worksheet.selectionIsUnique)
    //     selectedCell <- worksheet.selectionBase
    //   } {
    //     selectedCell.skeleton.zip(pasteExpr.ncell) match {
    //       case None => editor.consoleError("Selected cell has incompatible shape.")
    //       case Some(zippedTree) => {

    //         var itFits = true

    //         zippedTree map {
    //           case (cell, expr) => {
    //             if (! cell.isEmpty) {
    //               cell.item match {
    //                 case Neutral(Some(e)) => itFits &&= {
    //                   if (e convertsTo expr) true else {
    //                     editor.consoleError("Match error: expressions " ++ e.toString ++
    //                       " and " ++ expr.toString ++ " are not convertible.")
    //                     false
    //                   }
    //                 }
    //                 case _ => itFits = false
    //               }
    //             }
    //           }
    //         }

    //         if (itFits) {
    //           worksheet.deselectAll

    //           // BUG!!! - This refreshes the gallery on *every* assignment.  Need to delay that
    //           // somehow ....

    //           zippedTree map {
    //             case (cell, expr) => {
    //               if (cell.isEmpty) {
    //                 cell.item = Neutral(Some(expr))
    //               }
    //             }
    //           }
    //         } else {
    //           editor.consoleError("Pasting failed.")
    //         }
    //       }
    //     }
    //   }

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
