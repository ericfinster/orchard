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

abstract class Workspace(val module : ModuleSystem#Module) { thisWorkspace =>

  type EditorType <: Editor
  def editor : EditorType

  def activeWorksheet : Option[Worksheet]

  def stabilityLevel : Option[Int]
  def invertibilityLevel : Option[Int]
  def unicityLevel : Option[Int]

  //============================================================================================
  // SEMANTIC ROUTINES
  //

  def processIdentifier(rawIdent : RawIdentifier) : Option[List[Identifier]] = {
    // val idents = rawIdent.tokens flatMap {
    //   case RawLiteral(lit) => Some(LiteralIdentifier(lit))
    //   case RawReference(ref) =>
    //     variables find (p => p.id == ref) match {
    //       case None => { editor.consoleError("Unresolved reference: " ++ ref) ; None }
    //       case Some(v) => Some(v.ident)
    //     }
    // }

    // if (idents.length < rawIdent.tokens.length) {
    //   editor.consoleError("Identifier processing failed.")
    //   None
    // } else {
    //   Some(idents)
    // }
    ???
  }

  def assumeAtSelection(thinHint : Boolean) : Unit =
    for {
      worksheet <- activeWorksheet
      selectedCell <- worksheet.selectionBase
    } {

      try {

        val shell = new Shell(new WorkspaceFramework(selectedCell.neutralNCell))

        val forceThin =
          invertibilityLevel match {
            case None => false
            case Some(l) => selectedCell.dimension > l
          }

        editor.withAssumptionInfo(thinHint, forceThin,
          (identString, isThin) => {
            IdentParser(identString) match {
              case Success(ident, _) => {

                for {
                  idents <- processIdentifier(ident)
                } {

                  val finalIdent = CompoundIdentifier(idents)

                  // Make sure the identifier is unique
                  // if (variables exists (_.id == finalIdent.toString)) {
                  //   editor.consoleError("Duplicate Identifier: " ++ finalIdent.toString)
                  // } else {

                    val varExpr = ??? //Variable(shell, index, finalIdent, isThin)

                    worksheet.deselectAll
                    selectedCell.item = ???
                    worksheet.selectAsBase(selectedCell)

                    // Add the variable to the environment
                    // appendParameter(varExpr)
                  // }

                }
              }

              case _ : NoSuccess => editor.consoleError("Identifier parse failed.")
            }
          })

      } catch {
        case e : java.lang.AssertionError => 
          editor.consoleError("Cannot assume here: selection is not a shell.")
      }
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

            IdentParser(identString) match {
              case Success(ident, _) => {

                for {
                  idents <- processIdentifier(ident)
                } {

                  val finalIdent = CompoundIdentifier(idents)

                  // Make sure the identifier is unique
                  // if (variables exists (_.id == finalIdent.toString)) {
                  //   editor.consoleError("Duplicate Identifier: " ++ finalIdent.toString)
                  // } else {

                    val boundaryCell = selectedCell.boundaryFace
                    val filler = ??? //Filler(nook, finalIdent)

                    worksheet.deselectAll
                    selectedCell.item = ??? // Neutral(Some(filler))
                    boundaryCell.item = ??? // Neutral(Some(filler.Boundary))
                    worksheet.selectAsBase(selectedCell)

                  //appendLift(filler)
                  // }

                }
              }
              case _ : NoSuccess => editor.consoleError("Compose parse failed.")
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

  class Worksheet(seed : NCell[Polarity[ExpressionMarker]])
      extends AbstractWorksheet(seed) {

    type FrameworkType = Worksheet
    type CellType = WorksheetCell

    def stabilityLevel : Option[Int] = thisWorkspace.stabilityLevel
    def invertibilityLevel : Option[Int] = thisWorkspace.invertibilityLevel
    def unicityLevel : Option[Int] = thisWorkspace.unicityLevel

    def newCell(item : Polarity[ExpressionMarker]) = 
      new WorksheetCell(item)

    def extract(cell : CellType) =
      new Worksheet(cell.skeleton map (_.item))

    class WorksheetCell(itm : Polarity[ExpressionMarker]) 
        extends AbstractWorksheetCell(itm) {

    }
  }

  //============================================================================================
  // FRAMEWORKS
  //

  class WorkspaceFramework(seed : NCell[ExpressionMarker]) 
      extends Framework(seed) {

    type FrameworkType = WorkspaceFramework
    type CellType = WorkspaceFrameworkCell

    def stabilityLevel : Option[Int] = thisWorkspace.stabilityLevel
    def invertibilityLevel : Option[Int] = thisWorkspace.invertibilityLevel
    def unicityLevel : Option[Int] = thisWorkspace.unicityLevel

    def newCell(item : ExpressionMarker) =
      new WorkspaceFrameworkCell(item)

    def extract(cell : CellType) =
      new WorkspaceFramework(cell.skeleton map (_.item))

    class WorkspaceFrameworkCell(var item : ExpressionMarker)
        extends FrameworkCell {

      def expression : ExpressionMarker = item

      override def toString = item.toString

    }

  }

}
