/**
  * Workspace.scala - A workspace for manipulating expressions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.editor

import scala.collection.mutable.Map
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

import orchard.core.cell._
import orchard.core.complex._
import orchard.core.expression._

import IdentParser.Success
import IdentParser.NoSuccess

trait Workspace extends CheckableEnvironment with HasEnvironment {

  def name : String
  def editor : Editor

  override def toString = name

  type SubstitutionType <: Substitution

  val sheets = Buffer.empty[Worksheet]

  def newSheet : Unit 

  def activeSheet : Option[Worksheet]
  def activeExpression : Option[NCell[Expression]]
  def activeSubstitution : Option[SubstitutionType]

  def addToEnvironment(ncell : NCell[Expression]) : Unit = {
    envOps.appendElement(envRoot, ExpressionElement(ncell))
  }

  def addToEnvironment(node : EnvironmentNodeType) : Boolean = {
    // We would like to check for internal conflicts as well, so let's make a list of the 
    // imported identifiers as well as the conflicts and then print them out.

    val existingIdents = HashSet.empty[String]
    val duplicateIdents = HashSet.empty[String]

    // First add all the identifiers in the current environment
    existingIdents ++= envOps.toExprSeq(envRoot) map (_.id)

    envOps.foreachExpr(node, (expr => {
      if (existingIdents.contains(expr.id)) {
        duplicateIdents += expr.id
      } else {
        existingIdents += expr.id
      }
    }))

    if (duplicateIdents.size > 0) {
      duplicateIdents foreach (ident => println("Duplicate Identifier: " ++ ident))
      println("Environment import failed.")
      false
    } else {
      envOps.children(envRoot) += node
      true
    }
  }

  def definitionSnapshot : Definition = {
    val defEnvRoot = SimpleNodeImplementation.cloneFrom(envRoot, envOps)
    new Definition(defEnvRoot, stabilityLevel, invertibilityLevel, unicityLevel)
  }

  def newSubstitution(defn : Definition) : Unit = newSubstitutionInShell(defn, Object(None))
  def newSubstitutionInShell(defn : Definition, shell : NCell[Option[Expression]]) : Unit

  def closeSubstitution(subst : SubstitutionType) : Unit
  def importActiveSubstitution : Unit

  def cancelActiveSubstitution : Unit = 
    for {
      subst <- activeSubstitution
    } {
      closeSubstitution(subst)
    }

  def newSubstitutionInSelectedShell(defn : Definition) = 
    for {
      sheet <- activeSheet
      selectedCell <- sheet.selectionBase
    } {
      if (sheet.selectionIsShell || selectedCell.isComplete) {
        val shellFramework = selectedCell.framework
        shellFramework.topCell.item = None
        newSubstitutionInShell(defn, shellFramework.toCell)
      } else {
        println("Selection is not a shell.")
      }
    }

  def processIdentifier(ident : RawIdentifier) : Option[Identifier] = {
    if (ident.tokens.length > 0) {
      val newTokens = 
        ident.tokens flatMap {
          case RawLiteral(lit) => Some(LiteralToken(lit))
          case RawReference(ref) => {
            val opt = envOps.findById(envRoot, ref) map (node => 
              ExpressionToken(envOps.getExpression(node))
            )

            opt match {
              case None => println("Failed to find expression with id: " ++ ref)
              case Some(_) => ()
            }

            opt
          }
        }

      if (newTokens.length != ident.tokens.length) {
        println("Identifier conversion failed.")
        None
      } else {
        Some(Identifier(newTokens))
      }
    } else {
      println("Empty identifier.")
      None
    }
  }

  def assumeAtSelection(thinHint : Boolean) : Unit = 
    for {
      sheet <- activeSheet
      selectedCell <- sheet.selectionBase
    } {

      if (sheet.selectionIsShell) {

        val forceThin = 
          invertibilityLevel match {
            case None => false
            case Some(l) => selectedCell.dimension > l
          }

        editor.withAssumptionInfo(thinHint, forceThin,
          (identString, isThin) => {
            IdentParser(identString) match {
              case Success(ident, _) => {

                val finalIdent = processIdentifier(ident).get
                val varExpr = Variable(finalIdent, isThin)

                if (envOps.containsId(envRoot, finalIdent.toString)) {
                  println("Duplicate identifier.")
                } else {
                  sheet.deselectAll
                  selectedCell.item = Neutral(Some(varExpr))
                  addToEnvironment(selectedCell.framework.toCell map (_.get))
                  sheet.selectAsBase(selectedCell)
                }
              }

              case _ : NoSuccess => println("Identifier parse failed.")
            }
          })
      } else {
        println("Cannot assume here: selection is not a shell.")
      }
    }

  def fillAtSelection = 
    for {
      sheet <- activeSheet
      selectedCell <- sheet.selectionBase
    } {
      if (selectedCell.isUnicityFillable) {

        // editor.withFillerIdentifier(
        //   (fillerString => {
        //     IdentParser(fillerString) match {
        //       case Success(fillerIdent, _) => {

        //         // TODO : Check identifier is valid

        //         try {
        //           val contExpr = Contraction(selectedCell.neutralNCell, ??? /* fillerIdent */)

        //           sheet.deselectAll
        //           val newIdx = context.extendWith(contExpr)
        //           selectedCell.item = Neutral(newIdx)
        //           sheet.selectAsBase(selectedCell)
        //         } catch {
        //           case e : IllegalArgumentException => println("Duplicate identifier.")
        //         }
        //       }
        //       case _ : NoSuccess => println("Filler parse failed.")
        //     }
        //   }))

        ???

      } else if (selectedCell.isExposedNook) {

        editor.withFillerIdentifiers(
          (bdryString, fillerString) => {

            IdentParser(bdryString) match {
              case Success(bdryIdent, _) => {
                IdentParser(fillerString) match {
                  case Success(fillerIdent, _) => {

                    val bdryFinalIdent = processIdentifier(bdryIdent).get
                    val fillerFinalIdent = processIdentifier(fillerIdent).get

                    if (envOps.containsId(envRoot, bdryFinalIdent.toString) ||
                        envOps.containsId(envRoot, fillerFinalIdent.toString) ||
                        bdryFinalIdent.toString == fillerFinalIdent.toString) {
                      println("Duplicate identifier.")
                    } else {
                      val filler = Filler(fillerFinalIdent, bdryFinalIdent, selectedCell.isThinBoundary)

                      val boundaryCell =
                        if (selectedCell.isOutNook)
                          selectedCell.target.get
                        else
                          selectedCell.emptySources.head

                      // selectedCell.fullFaces foreach (face => {
                      //   face.item match {
                      //     case Neutral(Some(v @ Variable(ident, isThin))) => if (! isThin) addDependency(v, filler)
                      //     case Neutral(Some(bdry : Filler#Boundary)) => if (! bdry.isThin) addDependency(bdry, filler)
                      //     case _ => ()
                      //   }
                      // })

                      selectedCell.item = Neutral(Some(filler))
                      boundaryCell.item = Neutral(Some(filler.MyBoundary))

                      addToEnvironment(boundaryCell.framework.toCell map (_.get))
                      addToEnvironment(selectedCell.framework.toCell map (_.get))

                      sheet.clearAndSelect(selectedCell)
                    }
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

  // BUG!!! - There are constraints generated when pasting a loop which
  //          I think are not being checked.  (For example, try pasting 
  //          a non-loop f : x -> y into a loop on x and see what happens ...

  def pasteToSelection = 
    for {
      sheet <- activeSheet
      selectedCell <- sheet.selectionBase
      selectedExpr <- activeExpression
    } {
      if (sheet.selectionIsUnique) {

        selectedCell.skeleton.zip(selectedExpr) match {
          case None => println("Selected cell has incompativle shape.")
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
              sheet.deselectAll

              zippedTree map {
                case (cell, expr) => {
                  if (cell.isEmpty) {
                    cell.item = Neutral(Some(expr))
                  }
                }
              }
            } else {
              println("Cell does not fit.")
            }
          }
        }
      }
    }

  def renameActiveExpression = 
    for {
      expr <- activeExpression
    } {
      editor.withRenameIdentifier(expr.value,
        (idString => {

          IdentParser(idString) match {
            case Success(ident, _) => {

              val finalIdent = processIdentifier(ident).get

              println("New identifier: " ++ idString)

              if (envOps.containsId(envRoot, finalIdent.toString)) {
                println("Duplicate identifier")
              } else {

                expr.value match {
                  case v @ Variable(_, _) => v.ident = finalIdent
                  case f @ Filler(_, _, _) => f.ident = finalIdent
                  case bdry : Filler#Boundary => bdry.interior.bdryIdent = finalIdent
                }

                // BUG !! - Need to refresh a bunch of stuff so that the new
                //          identifier gets exported to all the sheets and the
                //          environment view
              }
            }
            case _ : NoSuccess => println("Identifier parse failed.")
          }
        })
      )
    }

  // def replaceInSheets(bindings : Map[Expression, Expression]) = {
  //   sheets foreach (sheet => {
  //     sheet.forAllCells(cell => {
  //       cell.item match {
  //         case Neutral(Some(e)) => {
  //           if (bindings.isDefinedAt(e)) {
  //             val newExpr = bindings(e)
  //             println("Replacing cell " ++ e.toString ++ " with " ++ newExpr.toString ++ " in sheet.")
  //             cell.item = Neutral(Some(newExpr))
  //           }
  //         }
  //         case _ => ()
  //       }
  //     })
  //   })
  // }

  class Worksheet(seed : NCell[Polarity[Option[Expression]]])
      extends AbstractWorksheet(seed)
      with CheckableFramework[Polarity[Option[Expression]]] {

    type CellType = WorksheetCell

    def newCell(item : Polarity[Option[Expression]]) = new WorksheetCell(item)
    def extract(cell : CellType) = new Worksheet(cell.skeleton map (_.item))

    class WorksheetCell(itm : Polarity[Option[Expression]])
        extends AbstractWorksheetCell
        with CheckableCell {

      protected var myItem = itm

      def item = myItem
      def item_=(newItm : Polarity[Option[Expression]]) = {
        val oldItem = item
        myItem = newItm
        emit(ChangeEvents.ItemChangedEvent(oldItem))
      }

      def expression : Option[Expression] = 
        item match {
          case Neutral(exprOpt) => exprOpt
          case _ => throw new IllegalArgumentException("Tried to get expression from polarized cell.")
        }

      def framework : WorkspaceFramework = 
        new WorkspaceFramework(skeleton map (_.expression))
    }
  }

  class WorkspaceFramework(seed : NCell[Option[Expression]])
      extends AbstractMutableComplex[Option[Expression]](seed)
      with Framework[Option[Expression]]
      with CheckableFramework[Option[Expression]] {

    type CellType = WorkspaceFrameworkCell

    def newCell(item : Option[Expression]) = new WorkspaceFrameworkCell(item)
    def extract(cell : CellType) = new WorkspaceFramework(cell.skeleton map (_.item))
    def emptyItem : Option[Expression] = None

    class WorkspaceFrameworkCell(var item : Option[Expression])
        extends AbstractMutableCell
        with FrameworkCell
        with CheckableCell {

      def expression : Option[Expression] = item
    }
  }
}
