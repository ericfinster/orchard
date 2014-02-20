/**
  * Workspace.scala - A workspace consisting of an environment and a collection of sheets
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.mutable.Buffer

import IdentParser.Success
import IdentParser.NoSuccess

trait Workspace extends Environment {

  override type EnvironmentSeqType <: Buffer[NCell[Expression]]

  // Okay. I think at the workspace level, we just want to have a collection of
  // *complexes* interacting with an *environment*.

  // The visual part of this setup should be on the user interface side.

  def name : String

  def stabilityLevel : Option[Int]
  def invertibilityLevel : Option[Int]
  def unicityLevel : Option[Int]

  val sheets = Buffer.empty[ExpressionWorksheet]

  def newSheet : Unit 
  def activeSheet : Option[ExpressionWorksheet]
  def activeExpression : Option[NCell[Expression]]

  def withAssumptionInfo(deps : Seq[NCell[Expression]],
                         thinHint : Boolean,
                         forceThin : Boolean,
                         handler : (String, Boolean) => Unit) : Unit 

  def assumeAtSelection(thinHint : Boolean) = 
    for {
      sheet <- activeSheet
      selectedCell <- sheet.selectionBase
    } {
      if (sheet.selectionIsShell) {
        val dependencies = selectionDependencies(sheet)

        val forceThin = 
          invertibilityLevel match {
            case None => false
            case Some(l) => selectedCell.dimension > l
          }

        withAssumptionInfo(dependencies, thinHint, forceThin,
          (identString, isThin) => {
            IdentParser(identString) match {
              case Success(ident, _) => {

                // TODO : Check if identifier is valid

                if (! environmentContains(ident.toString)) {
                  sheet.deselectAll
                  selectedCell.item = Neutral(Some(Variable(ident, isThin)))
                  environment += selectedCell.getSimpleFramework.toExpressionCell
                  sheet.selectAsBase(selectedCell)
                } else {
                  println("Duplicate identifier.")
                }
              }

              case _ : NoSuccess => println("Identifier parse failed.")
            }
          })
      } else {
        println("Cannot assume here: selection is not a shell.")
      }
    }

  def withFillerIdentifiers(deps : Seq[NCell[Expression]], handler : (String, String) => Unit) : Unit
  def withFillerIdentifier(handler : String => Unit) : Unit

  def fillAtSelection = 
    for {
      sheet <- activeSheet
      fillerCell <- sheet.selectionBase
    } {
      val dependencies = selectionDependencies(sheet)

      withFillerIdentifiers(dependencies,
        (targetString, fillerString) => {

          IdentParser(targetString) match {
            case Success(targetIdent, _) => {
              IdentParser(fillerString) match {
                case Success(fillerIdent, _) => {

                  // TODO : Check if identifiers are valid in the current context

                  //             def validRef(ident : IdentToken) : Boolean =
                  //               ident match {
                  //                 case LiteralToken(_) => true
                  //                 case ReferenceToken(id) => freeVars exists (expr => expr.value.id == id)
                  //               }

                  //             val validRefs = (true /: ((composeIdent.tokens ++ fillerIdent.tokens) map (validRef(_)))) (_&&_)

                  //             if (validRefs) {
                  //               // BUG: Does not check that the two are not given the *same* name ... and
                  //               // BUG: We shouldn't allow the empty string.

                  //               if (wksp.environmentContains(composeIdent.toString) ||
                  //                 wksp.environmentContains(fillerIdent.toString)) { println("Duplicate identifier.") ; None }
                  //               else Some(composeIdent, fillerIdent)
                  //             } else { println("Missing a variable.") ; None }
                  //           }

                  sheet.deselectAll

                  val filler = Filler(fillerIdent)
                  fillerCell.item = Neutral(Some(filler))

                  if (fillerCell.isOutNook) {
                    val targetCell = fillerCell.target.get
                    val targetIsThin = (true /: (fillerCell.sources.get map (_.isThin))) (_&&_)

                    targetCell.item = Neutral(Some(FillerFace(targetIdent, filler.id, targetIsThin)))
                    environment += targetCell.getSimpleFramework.toExpressionCell
                  } else {
                    val targetCell = fillerCell.emptySources.head
                    val targetIsThin = fillerCell.target.get.isThin
                    
                    targetCell.item = Neutral(Some(FillerFace(targetIdent, filler.id, targetIsThin)))
                    environment += targetCell.getSimpleFramework.toExpressionCell
                  }

                  environment += fillerCell.getSimpleFramework.toExpressionCell

                }
                case _ : NoSuccess => println("Filler parse failed.")
              }
            }
            case _ : NoSuccess => println("Compose parse failed.")
          }
        })
    }

  def expressionToSelection = 
    for {
      sheet <- activeSheet
      selectedCell <- sheet.selectionBase
      selectedExpr <- activeExpression
    } {
      if (sheet.selectionIsUnique) {
        selectedCell.skeleton.zip(selectedExpr) match {
          case None => println("Not compatible. Zip failed.")
          case Some(zippedTree) => {

            var itFits = true

            zippedTree map (pr => {
              val (eCell, e) = pr

              eCell.item match {
                case Neutral(None) => ()
                case Neutral(Some(f)) => if (itFits) { itFits &&= (e == f) } else ()
                case _ => itFits = false
              }
            })

            if (itFits) {
              sheet.deselectAll

              zippedTree map (pr => {
                val (eCell, e) = pr

                // This is overkill
                eCell.item = Neutral(Some(e))
              })
            } else {
              println("Cell does not fit.")
            }
          }
        }
      }
    }

  def selectionFreeVariables(sheet : ExpressionWorksheet) : Seq[NCell[Expression]] = {
    val freeVars = HashMap.empty[String, NCell[Expression]]

    sheet.selectedCells foreach (cell => {
      collectFreeVars(cell.getSimpleFramework, freeVars)
    })

    val values = freeVars.values

    environment filter (expr => values exists (e => e.value.id == expr.value.id))
  }

  def selectionDependencies(sheet : ExpressionWorksheet) : Seq[NCell[Expression]] = {
    val deps = HashMap.empty[String, NCell[Expression]]

    sheet.selectedCells foreach (cell => {
      collectFreeVars(cell.getSimpleFramework, deps)
    })

    val values = deps.values

    environment filter (expr => values exists (e => e.value.id == expr.value.id))
  }

}
