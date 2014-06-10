/**
  * Module.scala - Modules
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

import orchard.core.cell._
import orchard.core.complex._

import IdentParser.Success
import IdentParser.NoSuccess

trait Module extends ModuleEntry { thisModule : ModuleEnvironment#Module =>

  type VariableType = ModuleEnvironment#VariableType

  def activeWorksheet : Option[Worksheet]

  //============================================================================================
  // SEMANTIC ROUTINES
  //

  def processIdentifier(rawIdent : RawIdentifier, params : Seq[VariableType]) : Option[Identifier] = {
    val newTokens = rawIdent.tokens flatMap {
      case RawLiteral(lit) => Some(LiteralToken(lit))
      case RawReference(ref) =>
        params find (p => p.name == ref) match {
          case None => { editor.consoleError("Unresolved reference: " ++ ref) ; None }
          case Some(mv) => Some(ExpressionToken(mv.varExpr))
        }
    }

    if (newTokens.length < rawIdent.tokens.length) {
      editor.consoleError("Identifier processing failed.")
      None
    } else {
      Some(Identifier(newTokens))
    }
  }

  def assumeAtSelection(thinHint : Boolean) : Unit =
    for {
      worksheet <- activeWorksheet
      selectedCell <- worksheet.selectionBase
    } {

      try {

        val shell = new Shell(new ModuleFramework(selectedCell.neutralNCell))

        val forceThin =
          invertibilityLevel match {
            case None => false
            case Some(l) => selectedCell.dimension > l
          }

        val totalParameters = parameters ++ localParameters

        editor.withAssumptionInfo(thinHint, forceThin,
          (identString, isThin) => {
            IdentParser(identString) match {
              case Success(ident, _) => {

                for {
                  finalIdent <- processIdentifier(ident, totalParameters)
                } {

                  // Make sure the identifier is unique
                  if (totalParameters exists (_.name == finalIdent.toString)) {
                    editor.consoleError("Duplicate Identifier: " ++ finalIdent.toString)
                  } else {

                    val varExpr = Variable(shell, totalParameters.length, finalIdent, isThin)

                    worksheet.deselectAll
                    selectedCell.item = Neutral(Some(varExpr))
                    worksheet.selectAsBase(selectedCell)

                    // Add the variable to the environment
                    appendParameter(varExpr)
                  }

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

        val nook = new Nook(new ModuleFramework(selectedCell.neutralNCell))
        val totalParameters = parameters ++ localParameters

        editor.withFillerIdentifier(
          identString => {

            IdentParser(identString) match {
              case Success(ident, _) => {

                for {
                  finalIdent <- processIdentifier(ident, totalParameters)
                } {

                  // Make sure the identifier is unique
                  if (totalParameters exists (_.name == finalIdent.toString)) {
                    editor.consoleError("Duplicate Identifier: " ++ finalIdent.toString)
                  } else {

                    val boundaryCell = selectedCell.boundaryFace
                    val filler = Filler(nook, finalIdent)

                    worksheet.deselectAll
                    selectedCell.item = Neutral(Some(filler))
                    boundaryCell.item = Neutral(Some(filler.Boundary))
                    worksheet.selectAsBase(selectedCell)

                  }

                }
              }
              case _ : NoSuccess => println("Compose parse failed.")
            }
          })
      } else {
        editor.consoleError("Selection is not fillable.")
      }
    }
  
  def pasteToSelection(pasteExpr : Expression) =
    for {
      worksheet <- activeWorksheet
      if (worksheet.selectionIsUnique)
      selectedCell <- worksheet.selectionBase
    } {
      selectedCell.skeleton.zip(pasteExpr.ncell) match {
        case None => editor.consoleError("Selected cell has incompatible shape.")
        case Some(zippedTree) => {

          var itFits = true

          zippedTree map {
            case (cell, expr) => {
              if (! cell.isEmpty) {
                cell.item match {
                  case Neutral(Some(e)) => itFits &&= {
                    if (e == expr) true else {
                      println("Match error:")
                      println("e : " ++ e.toString)
                      println("expr : " ++ expr.toString)
                      false
                    }
                  }
                  case _ => itFits = false
                }
              }
            }
          }

          if (itFits) {
            worksheet.deselectAll

            // BUG!!! - This refreshes the gallery on *every* assignment.  Need to delay that
            // somehow ....

            zippedTree map {
              case (cell, expr) => {
                if (cell.isEmpty) {
                  cell.item = Neutral(Some(expr))
                }
              }
            }
          } else {
            editor.consoleError("Pasting failed.")
          }
        }
      }
    }

  //============================================================================================
  // WORKSHEETS
  //

  class Worksheet(seed : NCell[Polarity[Option[Expression]]])
      extends AbstractWorksheet(seed) {

    type CellType = WorksheetCell

    def stabilityLevel : Option[Int] = thisModule.stabilityLevel
    def invertibilityLevel : Option[Int] = thisModule.invertibilityLevel
    def unicityLevel : Option[Int] = thisModule.unicityLevel

    def newCell(item : Polarity[Option[Expression]]) = 
      new WorksheetCell(item)

    def extract(cell : CellType) =
      new Worksheet(cell.skeleton map (_.item))

    class WorksheetCell(itm : Polarity[Option[Expression]]) 
        extends AbstractWorksheetCell(itm) {

      def expression : Option[Expression] = 
        item match {
          case Neutral(exprOpt) => exprOpt
          case _ => ???
        }

    }
  }

  //============================================================================================
  // FRAMEWORKS
  //

  class ModuleFramework(seed : NCell[Option[Expression]]) 
      extends ExpressionFramework(seed) {

    type CellType = ModuleFrameworkCell

    def stabilityLevel : Option[Int] = thisModule.stabilityLevel
    def invertibilityLevel : Option[Int] = thisModule.invertibilityLevel
    def unicityLevel : Option[Int] = thisModule.unicityLevel

    def newCell(item : Option[Expression]) =
      new ModuleFrameworkCell(item)

    def extract(cell : CellType) =
      new ModuleFramework(cell.skeleton map (_.item))

    class ModuleFrameworkCell(var item : Option[Expression])
        extends ExpressionFrameworkCell(item)

  }

}
