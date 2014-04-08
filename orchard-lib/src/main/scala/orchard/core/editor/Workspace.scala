/**
  * Workspace.scala - A workspace for manipulating expressions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.editor

import scala.collection.mutable.Buffer

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

                    val filler = Filler(fillerFinalIdent, bdryFinalIdent, selectedCell.isThinFillerFace)

                    val boundaryCell =
                      if (selectedCell.isOutNook)
                        selectedCell.target.get
                      else
                        selectedCell.emptySources.head

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
          case None => println("Not compatible. Zip failed.")
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

  def substitute(varExpr : Expression, expr : Expression) = {
    println("Starting substitution: " ++ expr.toString " => " ++ varExpr.toString)

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
