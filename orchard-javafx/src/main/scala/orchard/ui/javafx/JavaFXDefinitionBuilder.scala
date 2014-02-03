/**
  * JavaFXDefinitionBuilder.scala - JavaFX implementation of a definiton builder
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.Includes._

import scalafx.scene.layout.StackPane

import scalafx.geometry.Side
import scalafx.geometry.Insets
import scalafx.geometry.Orientation

import scalafx.scene.control.Tab
import scalafx.scene.control.TabPane
import scalafx.scene.control.ListView
import scalafx.scene.control.SplitPane
import scalafx.scene.control.TitledPane
import scalafx.scene.control.Accordion

import scalafx.collections.ObservableBuffer

import javafx.event.Event
import javafx.event.EventHandler

import javafx.scene.input.KeyCode
import javafx.scene.input.KeyEvent
import javafx.scene.input.MouseEvent

import javafx.scene.{control => jfxsc}

import orchard.core._
import controls._

class JavaFXDefinitionBuilder(implicit pm : PopupManager) extends jfxsc.Tab with DefinitionBuilder {

  val environment = ObservableBuffer.empty[NCell[Expression]]

  var truncationLevel : Int = 0
  var contractibilityLevel : Int = 0

  case class Sheet(id : String, builder : ExpressionBuilder)

  val sheetPane = new StackPane 
  val sheets = ObservableBuffer.empty[Sheet]

  val controlAccordion = new Accordion

  val envListView = 
    new ListView[NCell[Expression]] {
      items = environment
      cellFactory = (_ => new EnvironmentCell)
    }

  val envPane = new TitledPane {
    text = "Environment"
    content = envListView
  }

  val substPane = new TitledPane {
    text = "Substitutions"
  }

  val shtPane = new TitledPane {
    text = "Sheets"
    content = new ListView[Sheet] {
      items = sheets
      cellFactory = (_ => new SheetCell)
    }
  }

  controlAccordion.panes = List(envPane, substPane, shtPane)

  val previewerPane = new StackPane {
    styleClass += "orch-pane"
  }

  val horizontalSplit = new SplitPane {
    orientation = Orientation.HORIZONTAL
    dividerPositions = 0.8f
  }

  horizontalSplit.items.addAll(
    (new StackPane { content = sheetPane; padding = Insets(10, 10, 10, 10); styleClass += "orch-pane" }).delegate,
    (new StackPane { content = controlAccordion; padding = Insets(10, 10, 10, 10); styleClass += "orch-pane" }).delegate
  )

  val verticalSplit = new SplitPane {
    orientation = Orientation.VERTICAL
    dividerPositions = 0.6f
  }

  verticalSplit.items.addAll(horizontalSplit, previewerPane)
  setContent(verticalSplit)

  class SheetCell extends jfxsc.ListCell[Sheet] {

    getStyleClass add "orch-list-cell"

    override def updateItem(sheet : Sheet, empty : Boolean) = {
      super.updateItem(sheet, empty)

      if (! empty) {
        setText(sheet.id)
      }
    }

    setOnMouseClicked(new EventHandler[MouseEvent] {
      def handle(ev : MouseEvent) {
        if (! isEmpty) {
          displaySheet(getItem)
        }
      }
    })

  }

  class EnvironmentCell extends jfxsc.ListCell[NCell[Expression]] {

    getStyleClass add "orch-list-cell"

    var lastStyle : Option[String] = None

    def setStyleType(styleType : String) = {
      lastStyle foreach (st => getStyleClass remove st)
      getStyleClass add styleType
      lastStyle = Some(styleType)
    }

    override def updateItem(expr : NCell[Expression], empty : Boolean) = {
      super.updateItem(expr, empty)

      if (! empty) {
        // Set the style based on the semantics ...
        expr.value match {
          case Variable(_, isThin) => {
            if (isThin) {
              setStyleType("orch-list-cell-var-thin")
            } else {
              setStyleType("orch-list-cell-var")
            }
          }
          case Filler(_, _) => setStyleType("orch-list-cell-filler")
          case FillerTarget(_, _, isThin) => {
            if (isThin) {
              setStyleType("orch-list-cell-filler-tgt-thin")
            } else {
              setStyleType("orch-list-cell-filler-tgt")
            }
          }
        }

        setText(expr.toString)
      }
    }

    setOnMouseClicked(new EventHandler[MouseEvent] {
      def handle(ev : MouseEvent) {
        if (! isEmpty) {
          setPreview(getItem)

          if (ev.getClickCount > 1) {
            newSheet(getItem map (e => Some(e)))
          }
        }
      }
    })

  }

  var sheetCount = 1

  def newSheet : Unit = newSheet(Object(None))

  def newSheet(seed : NCell[Option[Expression]]) = {
    val builder = new ExpressionBuilder(CardinalComplex(seed))
    val sheet = Sheet("Sheet " ++ sheetCount.toString, builder)
    sheets += sheet
    sheetCount += 1
    displaySheet(sheet)
  }

  def displaySheet(sheet : Sheet) = {
    sheetPane.content = sheet.builder
    sheet.builder.refreshAll
  }

  def activeBuilder : Option[ExpressionBuilder] = {
    val builder = sheetPane.content(0).asInstanceOf[ExpressionBuilder]
    if (builder != null) Some(builder) else None 
  }

  def envContains(id : String) : Boolean = {
    environment exists (expr => expr.value.id == id)
  }

  def setPreview(expr : NCell[Expression]) = {
    val gallery = new FrameworkGallery(expr map (e => Some(e)))
    gallery.renderAll
    previewerPane.content = gallery
    previewerPane.requestLayout
  }

  def assume(id : String, isThin : Boolean) = {
    for { 
      exprBuilder <- activeBuilder 
      emptyCell <- exprBuilder.selectionBase
    } {
      if (emptyCell.owner.isShell) {
        exprBuilder.deselectAll
        emptyCell.owner.item = Neutral(Some(Variable(id, isThin)))

        // To update the highlighting ...
        exprBuilder.refreshAll

        val exprCell : NCell[Expression] = emptyCell.owner.getSimpleFramework.toCell map (_.get)
        environment += exprCell

        exprBuilder.selectAsBase(emptyCell)
      }
    }
  }

  def fillExposedNook(nookCell : ExpressionBuilder#GalleryCell, targetId : String, fillerId : String) = {
    for {
      exprBuilder <- activeBuilder
    } {
      if (nookCell.owner.isExposedNook) {
        exprBuilder.deselectAll

        val nook = nookCell.owner.getSimpleFramework.getIdNook

        val (targetIsThin, targetCell) =
          if (nookCell.owner.isOutNook) {
            ((true /: (nookCell.owner.sources.get map (_.isThin))) (_&&_), nookCell.owner.target.get)
          } else {
            (nookCell.owner.target.get.isThin, nookCell.owner.emptySources.head)
          }

        targetCell.item = Neutral(Some(FillerTarget(targetId, nook, targetIsThin)))
        val tgtExprCell = targetCell.getSimpleFramework.toCell map (_.get)
        environment += tgtExprCell

        nookCell.owner.item = Neutral(Some(Filler(fillerId, nook)))
        val exprCell = nookCell.owner.getSimpleFramework.toCell map (_.get)
        environment += exprCell
      }
    }
  }

  def fillFromEnvironment(emptyCell : ExpressionBuilder#GalleryCell, expr : NCell[Expression]) = {
    for {
      exprBuilder <- activeBuilder
    } {
      val complex = exprBuilder.complex

      emptyCell.owner.skeleton.asInstanceOf[NCell[complex.ExpressionBuilderCell]]
        .zip(expr) match {
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
            exprBuilder.deselectAll

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

      // Overkill!!!
      exprBuilder.refreshAll
    }
  }

  newSheet
}
