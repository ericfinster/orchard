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

trait Workspace extends CheckableEnvironment {

  def name : String
  def editor : Editor

  override def toString = name

  val sheets = Buffer.empty[Worksheet]

  def newSheet : Unit 

  def activeSheet : Option[Worksheet]
  def activeExpression : Option[NCell[Expression]]

  val environment : GroupNode = GroupNode(name)

  val dependencyMap : Map[Expression, List[Expression]] = 
    new HashMap[Expression, List[Expression]]

  def addDependency(dep : Expression, filler : Expression) = {
    println("Adding dependency of " ++ filler.toString ++ " on " ++ dep.toString)

    if (dependencyMap.isDefinedAt(dep))
      dependencyMap(dep) = filler :: dependencyMap(dep)
    else
      dependencyMap(dep) = List(filler)
  }

  def addDependencies(node : EnvironmentNode) = {
    val seq = node.toSeq

    seq foreach (ncell => {
      ncell.value match {
        case filler @ Filler(_, _, _) => {
          // Make a framework, grab the faces and set the dependencies
          val framework = new WorkspaceFramework(ncell map (Some(_)))

          framework(framework.dimension - 1) foreachCell (cell => {
            cell.item match {
              case Some(v @ Variable(_, isThin)) => 
                if (! isThin) addDependency(v, filler)
              case Some(bdry : Filler#Boundary) => 
                if (bdry != filler.MyBoundary && ! bdry.isThin) addDependency(bdry, filler)
              case _ => ()
            }
          })
        }
        case _ => ()
      }
    })
  }

  def addToEnvironment(expr : NCell[Expression]) : EnvironmentNode = {
    val node = ExpressionNode(expr)
    environment.children += node
    node
  }

  def addToEnvironment(node : EnvironmentNode) : Unit = { 
    environment.children += node 
  }

  def dumpEnvironment : Unit = {
    val seq = environment.toSeq

    println("Environment (length = " ++ seq.length.toString ++ "): ")

    seq foreach (expr => {
      println(expr.value.toString)
    })
  }

  def templateSnapshot : Template = {
    new Template(EnvironmentNode.clone(environment).asInstanceOf[GroupNode])
  }

  def importTemplate(template : Template) = 
    importTemplateWithShell(template, Object(None))

  def importTemplateWithShell(template : Template, shell : NCell[Option[Expression]]) = {
    val newGroup = EnvironmentNode.clone(template.root) map (expr => {
      val shellFramework = new WorkspaceFramework(shell)
      shellFramework.stablyAppend(new WorkspaceFramework(expr map (Some(_))))
      shellFramework.toCell map (_.get)
    })

    addDependencies(newGroup)
    addToEnvironment(newGroup)
  }

  def importTemplateAtSelection(template : Template) = 
    for {
      sheet <- activeSheet
      selectedCell <- sheet.selectionBase
    } {
      if (sheet.selectionIsShell || selectedCell.isComplete) {
        val shellFramework = selectedCell.framework
        shellFramework.topCell.item = None
        importTemplateWithShell(template, shellFramework.toCell)
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
            val opt = environment.lookup(ref) map (expr => ExpressionToken(expr.value))

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

                sheet.deselectAll
                selectedCell.item = Neutral(Some(varExpr))
                addToEnvironment(selectedCell.framework.toCell map (_.get))
                sheet.selectAsBase(selectedCell)
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

                    val filler = Filler(fillerFinalIdent, bdryFinalIdent, selectedCell.isThinBoundary)

                    val boundaryCell =
                      if (selectedCell.isOutNook)
                        selectedCell.target.get
                      else
                        selectedCell.emptySources.head

                    selectedCell.fullFaces foreach (face => {
                      face.item match {
                        case Neutral(Some(v @ Variable(ident, isThin))) => if (! isThin) addDependency(v, filler)
                        case Neutral(Some(bdry : Filler#Boundary)) => if (! bdry.isThin) addDependency(bdry, filler)
                        case _ => ()
                      }
                    })

                    selectedCell.item = Neutral(Some(filler))
                    boundaryCell.item = Neutral(Some(filler.MyBoundary))

                    addToEnvironment(boundaryCell.framework.toCell map (_.get))
                    addToEnvironment(selectedCell.framework.toCell map (_.get))

                    sheet.clearAndSelect(selectedCell)
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

  def expressionToSelection = 
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

  def replaceInSheets(bindings : Map[Expression, Expression]) = {
    sheets foreach (sheet => {
      sheet.forAllCells(cell => {
        cell.item match {
          case Neutral(Some(e)) => {
            if (bindings.isDefinedAt(e)) {
              val newExpr = bindings(e)
              println("Replacing cell " ++ e.toString ++ " with " ++ newExpr.toString ++ " in sheet.")
              cell.item = Neutral(Some(newExpr))
            }
          }
          case _ => ()
        }
      })
    })
  }

  def replaceInEnvironment(bindings : Map[Expression, Expression]) = {

    // Could we use a foreach here so that we don't have to reset the
    // environment below there?

    val newEnv =
      environment map (ncell => {
        ncell map (ex => {
          if (bindings.isDefinedAt(ex)) {
            println("Replacing " ++ ex.toString ++ " in ncell " ++ ncell.value.toString)
            bindings(ex)
          } else ex
        })
      })

    environment.children.clear
    environment.children ++= newEnv.asInstanceOf[GroupNode].children
  }

  def replaceInIdentifiers(bindings : Map[Expression, Expression]) = {
    environment.toExprSeq foreach (ex => {
      if (ex.ident.exprRefs exists (ref => bindings.isDefinedAt(ref))) {
        println("Expression " ++ ex.toString ++ " references a bound variable.")

        ex.ident.tokens foreach {
          case et @ ExpressionToken(e) => if (bindings.isDefinedAt(e)) { et.expr = bindings(e) }
          case _ => ()
        }
      }
    })
  }

  def substitute(varExpr : NCell[Expression], expr : NCell[Expression]) = {
    println("Starting substitution: " ++ expr.value.toString ++ " => " ++ varExpr.value.toString)

    varExpr.zip(expr) match {
      case None => println("Selected cell has incompatible shape.")
      case Some(zippedTree) => {
        println("Shape is compatible.")

        val bindings = HashMap.empty[Expression, Expression]
        val thinDeps = HashSet.empty[Expression]

        var statusOk : Boolean = true

        zippedTree map {
          case (tgtExpr, srcExpr) => {
            if (tgtExpr == srcExpr) {
              println("Cell match for: " ++ tgtExpr.toString)
            } else {
              tgtExpr match {
                case Variable(ident, isThin) => {
                  println("Attempting subordinate bind: " ++ srcExpr.toString ++ " => " ++ ident.toString)

                  if (bindings.isDefinedAt(tgtExpr)) {
                    if (bindings(tgtExpr) != srcExpr) {
                      println("Variable " ++ ident.toString ++ " is already bound to " ++ bindings(tgtExpr).toString)
                      statusOk = false
                    }
                  }

                  bindings(tgtExpr) = srcExpr

                  if (isThin && ! srcExpr.isThin) {
                    println("Cannot bind " ++ srcExpr.toString ++ " to thin variable " ++ ident.toString)
                    statusOk = false
                  }

                  if (! isThin && srcExpr.isThin) {
                    println("Variable " ++ ident.toString ++ " substituted with thin expression " ++ srcExpr.toString)
                    thinDeps += tgtExpr
                  }
                }
                case _ => {
                  println("Cell " ++ tgtExpr.toString ++ " is rigid and cannot be substituted for.")
                  statusOk = false
                }
              }
            }
          }
        }

        if (statusOk) {
          println("Look's like we're a go ...")

          bindings.keySet.foreach (key => {
            println("Deleting " ++ key.toString ++ "@" ++ key.hashCode.toString)
            environment.locateNode(key).get.delete
          })

          replaceInSheets(bindings)
          replaceInEnvironment(bindings)
          replaceInIdentifiers(bindings)

          // Now recheck necessary cells for universality

          while (thinDeps.size > 0) {
            val newDeps = new HashSet[Expression]

            thinDeps foreach (dep => {
              println("Rechecking universality dependents for " ++ dep.toString)

              if (dependencyMap.isDefinedAt(dep)) {
                dependencyMap(dep) foreach (d => {
                  val ncell = environment.locateNode(d).get.expr
                  val filler = ncell.value.asInstanceOf[Filler]
                  val framework = new WorkspaceFramework(ncell map (Some(_)))

                  // Clear the framework to nook state
                  framework.topCell.item = None
                  framework(framework.dimension - 1) foreachCell (cell => {
                    if (cell.item == Some(filler.MyBoundary)) cell.item = None
                  })

                  if (! framework.topCell.isExposedNook)
                    throw new IllegalStateException("Found an ill-defined filler.")

                  if (framework.topCell.isThinBoundary && ! filler.MyBoundary.isThin) {
                    println("Boundary " ++ filler.MyBoundary.toString ++ " has become thin.")

                    filler.bdryIsThin = true
                    newDeps += filler.MyBoundary
                  }
                })
              }

              thinDeps -= dep
            })

            thinDeps ++= newDeps
          }
        }
      }
    }
  }

  def abstractExpression(filler : Filler) = {
    val bindings : Map[Expression, Expression] = new HashMap

    val fillerVariable = Variable(filler.ident, true)
    val boundaryVariable = Variable(filler.bdryIdent, filler.bdryIsThin)

    bindings(filler) = fillerVariable
    bindings(filler.MyBoundary) = boundaryVariable

    replaceInSheets(bindings)
    replaceInEnvironment(bindings)
    replaceInIdentifiers(bindings)

    // What about the environment nodes?  I think it's okay ...
  }

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
