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

import Environment._

trait Workspace {

  type EnvironmentSeqType <: Buffer[NCell[Expression]]

  def name : String

  def environment : EnvironmentSeqType

  def editor : Editor

  def stabilityLevel : Option[Int]
  def invertibilityLevel : Option[Int]
  def unicityLevel : Option[Int]

  val sheets = Buffer.empty[ExpressionWorksheet]

  def newSheet : Unit 
  def activeSheet : Option[ExpressionWorksheet]
  def activeExpression : Option[NCell[Expression]]

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

        editor.withAssumptionInfo(dependencies, thinHint, forceThin,
          (identString, isThin) => {
            IdentParser(identString) match {
              case Success(ident, _) => {

                // TODO : Check if identifier is valid (including whether they
                //        are empty or equal!)

                if (! environment.contains(ident.toString)) {
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

  def composeAtSelection = 
    for {
      sheet <- activeSheet
      base <- sheet.selectionBase
    } {
      if (sheet.selectionIsComposable) {
        val dependencies = selectionDependencies(sheet)

        editor.withFillerIdentifiers(dependencies,
          (targetString, fillerString) => {

            IdentParser(targetString) match {
              case Success(targetIdent, _) => {
                IdentParser(fillerString) match {
                  case Success(fillerIdent, _) => {

                    // TODO : Check that the identifiers are valid.

                    val targetIsThin = 
                      ((true /: (sheet.selectedCells map (_.isThin))) (_&&_)) || (
                        invertibilityLevel match {
                          case None => false
                          case Some(l) => base.dimension > l
                        })

                    val fillerExpr = Filler(fillerIdent)
                    val targetExpr = FillerFace(targetIdent, fillerExpr.id, targetIsThin)

                    // Extrude and fill in the results
                    sheet.extrudeAtSelection(Some(targetExpr), Some(fillerExpr))

                    val targetCell = sheet.selectionBase.get
                    val fillerCell = targetCell.incoming.get

                    environment += targetCell.getSimpleFramework.toExpressionCell
                    environment += fillerCell.getSimpleFramework.toExpressionCell

                  }
                  case _ : NoSuccess => println("Filler parse failed.")
                }
              }
              case _ : NoSuccess => println("Compose parse failed.")
            }
          })
      }
    }

  def fillAtSelection = 
    for {
      sheet <- activeSheet
      fillerCell <- sheet.selectionBase
    } {
      val isUnicityFillable = fillerCell.isShell && (
        unicityLevel match {
          case None => false
          case Some(l) => fillerCell.dimension > l
        })

      if (isUnicityFillable) {
        val dependencies = selectionDependencies(sheet)

        editor.withFillerIdentifier(dependencies,
          (fillerString => {
            IdentParser(fillerString) match {
              case Success(fillerIdent, _) => {

                // TODO : Check identifier is valid

                if (! environment.contains(fillerIdent.toString)) {
                  sheet.deselectAll
                  fillerCell.item = Neutral(Some(Variable(fillerIdent, true)))  // Uh ... need a new expression type here
                  environment += fillerCell.getSimpleFramework.toExpressionCell
                  sheet.selectAsBase(fillerCell)
                } else {
                  println("Duplicate identifier.")
                }

              }
              case _ : NoSuccess => println("Filler parse failed.")
            }
          }))

      } else if (fillerCell.isExposedNook) {
        val dependencies = selectionDependencies(sheet)

        editor.withFillerIdentifiers(dependencies,
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

                    val filler = Filler(fillerIdent)
                    val thinByInvertibility = 
                      invertibilityLevel match {
                        case None => false
                        case Some(l) => (fillerCell.dimension - 1) > l
                      }

                    val (targetCell, targetIsThin) = 
                      if (fillerCell.isOutNook) {
                        (fillerCell.target.get, (true /: (fillerCell.sources.get map (_.isThin))) (_&&_) || thinByInvertibility)
                      } else {
                        (fillerCell.emptySources.head, fillerCell.target.get.isThin || thinByInvertibility)
                      }

                    fillerCell.item = Neutral(Some(filler))
                    targetCell.item = Neutral(Some(FillerFace(targetIdent, filler.id, targetIsThin)))

                    environment += targetCell.getSimpleFramework.toExpressionCell
                    environment += fillerCell.getSimpleFramework.toExpressionCell

                    sheet.clearAndSelect(fillerCell)
                  }
                  case _ : NoSuccess => println("Filler parse failed.")
                }
              }
              case _ : NoSuccess => println("Compose parse failed.")
            }
          })
      } else {
        println("Selection is not fillable.")
      }
    }

  def identityAtSelection = 
    for {
      sheet <- activeSheet
      selectedCell <- sheet.selectionBase
    } {
      if (sheet.selectionIsDroppable) {
        val dependencies = selectionDependencies(sheet)

        editor.withFillerIdentifiers(dependencies,
          (targetString, fillerString) => {

            IdentParser(targetString) match {
              case Success(targetIdent, _) => {
                IdentParser(fillerString) match {
                  case Success(fillerIdent, _) => {

                    val fillerExpr = Filler(fillerIdent)
                    val targetExpr = FillerFace(targetIdent, fillerExpr.id, true) 

                    // Extrude and fill in the results
                    sheet.dropAtSelection(Some(targetExpr), Some(fillerExpr))

                    val targetCell = sheet.selectionBase.get
                    val fillerCell = targetCell.incoming.get

                    environment += targetCell.getSimpleFramework.toExpressionCell
                    environment += fillerCell.getSimpleFramework.toExpressionCell
                  }
                  case _ : NoSuccess => println("Filler parse failed.")
                }
              }
              case _ : NoSuccess => println("Compose parse failed.")
            }
          })
      } else {
        println("Cannot insert identity here.")
      }
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

  def selectionDependencies(sheet : ExpressionWorksheet) : Seq[NCell[Expression]] = {
    val ds = Buffer.empty[NCell[Expression]]

    sheet.selectedCells foreach (cell => {
      cell.item match {
        case Neutral(Some(ex @ _)) => 
          environment.deps(ex.id, ds)
        case Neutral(None) => {
          val framework = cell.getSimpleFramework

          framework.topCell.fullFaces foreach (
            _.item match {
              case Some(e) => {
                environment.deps(e.id, ds)
                if (! ds.contains(e.id)) { ds += environment.lookup(e.id).get }
              }
              case None => ()
            }
          )
        }
        case _ => ()
      }
    })

    ds
  }

  def selectionFreeVariables(sheet : ExpressionWorksheet) : Seq[NCell[Expression]] = {
    selectionDependencies(sheet) filter (expr =>
      expr.value match {
        case Variable(_, _) => true
        case _ => false
      }
    )
  }

  trait CheckableFramework[A] extends ExpressionFramework[A] {

    type CellType <: CheckableFrameworkCell

    trait CheckableFrameworkCell extends ExpressionFrameworkCell { thisCell : CellType =>

      def isUnicityFillable : Boolean = 
        unicityLevel match {
          case None => false
          case Some(l) => dimension > l
        }

      def isFillable : Boolean = 
        if (isUnicityFillable) true else isExposedNook

      // For an exposed nook, determine if the filler face is thin
      def isThinFillerFace : Boolean = {
        val thinByInvertibility =
          invertibilityLevel match {
            case None => false
            case Some(l) => (dimension - 1) > l
          }

        if (isOutNook) {
          (true /: (sources.get map (_.isThin))) (_&&_) || thinByInvertibility
        } else {
          target.get.isThin || thinByInvertibility
        }
      }

      def getFramework : Framework = 
        new Framework(skeleton map (_.exprItem))
    }

  }

  class Framework(seed : NCell[Option[Expression]]) 
      extends AbstractFramework(seed) 
      with CheckableFramework[Option[Expression]] {

    type CellType = FrameworkCell

    def newCell(item : Option[Expression]) = new FrameworkCell(item)

    class FrameworkCell(item : Option[Expression]) 
        extends AbstractFrameworkCell(item) 
        with CheckableFrameworkCell {

      // From here we can implement methods which check whether or not a given cell is fillable!!

      // And secondly, we should be able to implement much better dependency tracking, since now
      // the cells themselves have access to both the environment and the filling settings.

    }
  }

  class Worksheet(seed : NCell[Polarity[Option[Expression]]]) 
      extends AbstractExpressionWorksheet(seed) 
      with CheckableFramework[Polarity[Option[Expression]]] {

    type CellType = WorksheetCell

    def newCell(item : Polarity[Option[Expression]]) = new WorksheetCell(item)

    def selectionDependencies : Seq[NCell[Expression]] = ???
    def selectionFreeVariables : Seq[NCell[Expression]] = ???

    class WorksheetCell(item : Polarity[Option[Expression]]) 
        extends AbstractExpressionWorksheetCell(item) 
        with CheckableFrameworkCell {

    }

  }
}
