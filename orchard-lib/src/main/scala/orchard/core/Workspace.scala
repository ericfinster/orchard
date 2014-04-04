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

trait Workspace extends ContextEnvironment { thisWorkspace =>

  type EditorType <: Editor

  def name : String
  def editor : EditorType

  override def toString = name

  val sheets = Buffer.empty[Worksheet]

  def newSheet : Unit 
  def activeSheet : Option[Worksheet]
  def activeExpression : Option[Expression[IndexType]]
  def activeExpressionIndex : IndexType

  def processIdentifier(ident : Identifier) : Option[IndexedIdentifier[IndexType]] = {
    val fvm = context.freeVariableMap

    try {

      if (ident.tokens.length > 0) {
        val tokens : List[IndexedIdToken[IndexType]] =
          ident.tokens map {
            case LiteralToken(lit) => StringToken[IndexType](lit)
            case ReferenceToken(id) => IndexToken[IndexType](fvm(id))
          }

        Some(IndexedIdentifier(tokens))
      } else {
        None
      }

    } catch {
      case e : NoSuchElementException => { println("Unresolved reference.") ; None }
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

                val idxdIdent = processIdentifier(ident).get

                try {
                  val varExpr = Variable(selectedCell.neutralNCell, idxdIdent, isThin)

                  sheet.deselectAll
                  val newIdx = context.extendWith(varExpr)
                  selectedCell.item = Neutral(newIdx)
                  sheet.selectAsBase(selectedCell)
                } catch {
                  case e : IllegalArgumentException => println("Duplicate identifier.")
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

        editor.withFillerIdentifier(
          (fillerString => {
            IdentParser(fillerString) match {
              case Success(fillerIdent, _) => {

                // TODO : Check identifier is valid

                try {
                  val contExpr = Contraction(selectedCell.neutralNCell, ??? /* fillerIdent */)

                  sheet.deselectAll
                  val newIdx = context.extendWith(contExpr)
                  selectedCell.item = Neutral(newIdx)
                  sheet.selectAsBase(selectedCell)
                } catch {
                  case e : IllegalArgumentException => println("Duplicate identifier.")
                }
              }
              case _ : NoSuccess => println("Filler parse failed.")
            }
          }))

      } else if (selectedCell.isExposedNook) {

        editor.withFillerIdentifiers(
          (targetString, fillerString) => {

            IdentParser(targetString) match {
              case Success(targetIdent, _) => {
                IdentParser(fillerString) match {
                  case Success(fillerIdent, _) => {

                    val targetIdxdIdent = processIdentifier(targetIdent).get
                    val fillerIdxdIdent = processIdentifier(fillerIdent).get

                    try {

                      val (bdryIdx, intrIdx) = context.nextIndexPair

                      val bdryExpr = Boundary[IndexType](intrIdx, targetIdxdIdent, selectedCell.isThinFillerFace)
                      val interExpr = Interior[IndexType](bdryIdx, selectedCell.neutralNCell, fillerIdxdIdent)

                      context.extendWith(bdryExpr)
                      context.extendWith(interExpr)

                      val boundaryCell = 
                        if (selectedCell.isOutNook)
                          selectedCell.target.get
                        else
                          selectedCell.emptySources.head

                      selectedCell.item = Neutral(intrIdx)
                      boundaryCell.item = Neutral(bdryIdx)

                      sheet.clearAndSelect(selectedCell)

                    } catch {
                      case e : IllegalArgumentException => println("Duplicate identifier.")
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

  def expressionToSelection = 
    for {
      sheet <- activeSheet
      selectedCell <- sheet.selectionBase
    } {

      // Okay, the other thing we need is to be able to extract the NCell corresponding
      // to any kind of expression

      if (sheet.selectionIsUnique) {

        val exprNCell = ShapeFramework(activeExpressionIndex).toCell 

        selectedCell.skeleton.zip(exprNCell) match {
          case None => println("Not compatible. Zip failed.")
          case Some(zippedTree) => {

            var itFits = true

            zippedTree map {
              case (cell, seq) => {
                if (! cell.isEmpty) {
                  cell.item match {
                    case Neutral(s) => itFits &&= (s == seq)
                    case _ => itFits = false
                  }
                }
              }
            }

            if (itFits) {
              sheet.deselectAll

              zippedTree map {
                case (cell, seq) => {
                  if (cell.isEmpty) {
                    cell.item = Neutral(seq)
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


  abstract class Worksheet(seed : NCell[Polarity[IndexType]])
      extends ExtrudableComplex[IndexType](seed) 
      with CheckableFramework[Polarity[IndexType]] {

    type CellType <: WorksheetCell
    type ExprIndexType = IndexType

    abstract class WorksheetCell(itm : Polarity[IndexType])
        extends ExtrudableCell(itm) 
        with CheckableCell { thisCell : CellType =>
    }

  }


}
