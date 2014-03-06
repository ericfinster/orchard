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

  val sheets = Buffer.empty[Worksheet]

  def newSheet : Unit 
  def activeSheet : Option[Worksheet]
  def activeExpression : Option[NCell[Expression]]

  def assumeAtSelection(thinHint : Boolean) = 
    for {
      sheet <- activeSheet
      selectedCell <- sheet.selectionBase
    } {
      if (sheet.selectionIsShell) {
        val dependencies = sheet.selectionDependencies

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
        val dependencies = sheet.selectionDependencies

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
        val dependencies = sheet.selectionDependencies

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
        val dependencies = sheet.selectionDependencies

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
        val dependencies = sheet.selectionDependencies

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

    def dependencies(env : Seq[NCell[Expression]]) : Seq[NCell[Expression]] = {
      val deps = HashMap.empty[String, NCell[Expression]]
      collectDependencies(env, deps)
      deps.values.toSeq
    }

    override def clone : Framework = new Framework(this.toCell)

    def collectDependencies(env : Seq[NCell[Expression]], deps : HashMap[String, NCell[Expression]]) : Unit = {

      def processCell(id : String, cell : CellType) = {
        if (! deps.isDefinedAt(id)) {

          deps(id) = cell.toExpressionCell

          cell.item match {
            case Some(Filler(_)) => {
              cell.getFillerFace foreach (face =>
                face.item match {
                  case Some(FillerFace(ident, _, _)) =>
                    if (! deps.isDefinedAt(ident.toString))
                      deps(ident.toString) = face.toExpressionCell
                  case _ => () // This should be an error
                }
              )
            }
            case Some(FillerFace(_, filler, _)) => {
              if (! deps.isDefinedAt(filler))
                deps(filler) = env.lookup(filler).get
            }
            case _ => ()
          }

          cell.getFramework.collectDependencies(env, deps)
        }
      }

      if (dimension > 0) {
        topCell.item match {
          case Some(FillerFace(_, filler, _)) => {
            Framework(env.lookup(filler).get).collectDependencies(env, deps)
          }
          case Some(Filler(ident)) => {
            baseCells(dimension - 1) foreachCell (cell => {
              cell.item foreach (e => {
                e match {
                  case FillerFace(_, filler, _) => 
                    if (filler != ident.toString) processCell(e.id, cell)
                  case _ => processCell(e.id, cell)
                }
              })
            })
          }
          case _ => {
            baseCells(dimension - 1) foreachCell (cell => {
              cell.item foreach (e => processCell(e.id, cell))
            })
          }
        }
      }
    }

    class FrameworkCell(i : Option[Expression]) 
        extends AbstractFrameworkCell(i) 
        with CheckableFrameworkCell {

      def getFillerFace : Option[CellType] = {
        var result : Option[CellType] = None

        item match {
          case Some(Filler(ident)) => {
            println("Looking for face of: " ++ ident.toString)
            fullFaces foreach (face => {
              face.item foreach {
                case FillerFace(_, filler, _) =>
                  if (filler == ident.toString)
                    result = Some(face)
                case _ => ()
              }
            })
          }
          case _ => ()
        }

        result
      }
    }
  }

  object Framework {
    def apply(seed : NCell[Expression]) = new Framework(seed map (Some(_)))
  }

  class Worksheet(seed : NCell[Polarity[Option[Expression]]]) 
      extends AbstractExpressionWorksheet(seed) 
      with CheckableFramework[Polarity[Option[Expression]]] {

    type CellType = WorksheetCell

    def newCell(item : Polarity[Option[Expression]]) = new WorksheetCell(item)

    class WorksheetCell(i : Polarity[Option[Expression]]) 
        extends AbstractExpressionWorksheetCell(i) 
        with CheckableFrameworkCell {

    }

    def selectionDependencies : Seq[NCell[Expression]] = {
      val deps = HashMap.empty[String, NCell[Expression]]

      selectedCells foreach (cell => {
        val framework = cell.getFramework

        if (cell.exprItem != None)
          framework.glob(None, None)

        framework.collectDependencies(environment, deps)
      })

      val envDeps = Buffer.empty ++= environment
      envDeps filter (expr => deps.isDefinedAt(expr.value.id))
    }

    def selectionFreeVariables : Seq[NCell[Expression]] =
      selectionDependencies.vars

  }
}
