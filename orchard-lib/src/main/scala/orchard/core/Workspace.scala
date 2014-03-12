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

  def assumeAtSelection(thinHint : Boolean) : Unit = 
    for {
      sheet <- activeSheet
      selectedCell <- sheet.selectionBase
    } {
      if (this.isInstanceOf[SubstitutionWorkspace]) {
        println("Cannot make new assumptions during a substitution.")
        return
      }

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

                if (! environment.containsId(ident.toString)) {
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

                if (! environment.containsId(fillerIdent.toString)) {
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

  def unfold(app : Application) : Seq[NCell[Expression]] = {

    val imports = Buffer.empty[NCell[Expression]]

    // It would be nice to have the dependencies around so that we can
    // make multiple passes through the environment until we have finished...

    val openDependents = HashMap.empty[String, Seq[String]]
    
    app.defn.environment foreach (expr => {
      expr.value match {
        case Variable(_, _) => ()
        case FillerFace(_, _, _) => ()
        case e @ _ =>
          openDependents(e.id) = Framework(expr).dependencies(app.defn.environment) map (_.value.id)
      }
    })

    // Create a set of bindings to work from
    val bindings = HashMap.empty[String, Expression] ++ app.bindings

    def importPass : Seq[NCell[Expression]] = {

      val completedDependents = Buffer.empty[NCell[Expression]]

      openDependents foreach { case (depId, depSeq) => {
        if (depSeq forall (bindings.isDefinedAt(_))) {

          println("Completed dependent: " ++ depId)

          // First we build a framework for the new dependent
          val dependentFramework = new Framework(app.shell)
          val startDim = dependentFramework.dimension
          dependentFramework.stablyAppend(Framework(app.defn.environment.lookup(depId).get))
          val endDim = dependentFramework.dimension + 1

          def translateIds =
            dependentFramework forAllCells(startDim, endDim,
              (cell => {
                cell.item foreach (e => {
                  if (bindings.isDefinedAt(e.id)) {
                    cell.item = Some(bindings(e.id))
                  } else {
                    println("Missing identifier " ++ e.id ++ " in translation.")
                  }
                })
              })
            )

          dependentFramework.topCell.item match {
            case Some(Filler(_)) => {
              // A filler which as a face
              val filler = dependentFramework.topCell
              val ff = dependentFramework.topCell.getFillerFace.get

              // Save the items for use later
              val fillerItem = filler.item.get.asInstanceOf[Filler]
              val ffItem = ff.item.get.asInstanceOf[FillerFace]

              // Clear the parts which will be replaced
              filler.item = None
              ff.item = None

              // Translate the rest via the bindings
              translateIds

              // At this point, we should have an exposed nook corresponding to the
              // lift which this depenedent represents
              if (filler.isExposedNook) {
                val newFillerItem = Filler(fillerItem.ident.translateWithBindings(bindings))
                val newFfItem = FillerFace(ffItem.ident.translateWithBindings(bindings), newFillerItem.id, filler.isThinFillerFace)

                filler.item = Some(newFillerItem)
                ff.item = Some(newFfItem)

                // Now we need to set the bindings ...
                bindings(fillerItem.id) = newFillerItem
                bindings(ffItem.id) = newFfItem

                // And tag them as completed
                completedDependents += ff.toExpressionCell
                completedDependents += filler.toExpressionCell
              } else {
                throw new IllegalArgumentException("After translation, we don't have a nook!")
              }
            }
            case Some(UnicityFiller(_)) => {
              val filler = dependentFramework.topCell
              val fillerItem = filler.item.get.asInstanceOf[UnicityFiller]
              filler.item = None

              translateIds

              if (filler.isUnicityFillable) {
                val newFillerItem = UnicityFiller(fillerItem.ident.translateWithBindings(bindings))
                filler.item = Some(newFillerItem)
                bindings(fillerItem.id) = newFillerItem
                completedDependents += filler.toExpressionCell
              } else {
                throw new IllegalArgumentException("After translate, we are not unicity fillable!")
              }
            }
            case Some(appItem @ Application(_, _, _)) => {
              // Hmmm ... perhaps we are overwriting a binding here ....
              bindings(appItem.id) = appItem
              translateIds

              // Any kind of sanity check or rebinding necessary here???

              completedDependents += dependentFramework.toExpressionCell
            }
            case _ => println("Unexpected topCell in expansion.")
          }

          // Remove this guy from the list of opens ...
          openDependents -= depId
        }
      }}

      completedDependents
    }

    var lastLength : Int = imports.length

    do {
      println("Entering import pass ...")
      lastLength = imports.length
      imports ++= importPass
      println("Imported " ++ lastLength.toString ++ " dependents")
    } while (lastLength != imports.length)

    if (openDependents.size > 0) {
      println("There were unresolved dependents ...")
    }

    // // Remove this application from the current environment
    // environment -= environment.lookup(app.id).get

    // // Now add the new imports
    // imports foreach (expr => {
    //   if (! environment.containsId(expr.value.id)) {
    //     environment += expr

    //     if (expr.value.id == app.id) {
    //       // Go throught all the sheets and update the cells so that the expanded expression 
    //       // is changed to it's output value
    //       val appDim : Int = expr.dimension.toInt

    //       sheets foreach (sheet => {
    //         sheet(appDim) foreachCell (cell => {
    //           cell.item match {
    //             case Neutral(Some(e)) => if (e.id == expr.value.id) { cell.item = Neutral(Some(expr.value)) }
    //             case _ => ()
    //           }
    //         })
    //       })

    //       // BUG!!! - We also have to go through the environment, since there might be cells which also
    //       //          have this guy as a face ....
    //       val changedExprs = Buffer.empty[NCell[Expression]]

    //       environment foreach (expr => {
    //         if (expr.dimension.toInt >= appDim) {
    //           val framework = Framework(expr)

    //           framework(appDim) foreachCell (cell => {
    //             cell.item match {
    //               case Some(e) => if (e.id == expr.value.id) { cell.item = Some(expr.value) ; changedExprs += expr }
    //               case _ => ()
    //             }
    //           })
    //         }
    //       })

    //       changedExprs foreach (expr => {
    //         val idx = environment.indexWhere(e => e.value.id == expr.value.id)
    //         environment(idx) = expr
    //       })
    //     }
    //   } else {
    //     println("Skipping import due to name clash for: " ++ expr.value.id)
    //   }
    // })

    imports
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
            val startDim = 
              if (topCell.isDrop) (dimension - 2) else (dimension - 1)

            baseCells(startDim) foreachCell (cell => {
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
