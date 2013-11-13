/**
  * Editor.scala - Main module for JavaFX Orchard Editor
  * 
  * @author Eric Finster
  * @version 0.1
  */

package orchard.ui.javafx

import scalafx.Includes._

import scalafx.scene.Scene

import scalafx.scene.layout.HBox
import scalafx.scene.layout.VBox
import scalafx.scene.layout.GridPane
import scalafx.scene.layout.StackPane
import scalafx.scene.layout.AnchorPane
import scalafx.scene.layout.Priority

import scalafx.scene.control.Button
import scalafx.scene.control.Accordion
import scalafx.scene.control.TitledPane
import scalafx.scene.control.SplitPane
import scalafx.scene.control.TabPane
import scalafx.scene.control.Tab
import scalafx.scene.control.TableView
import scalafx.scene.control.TableColumn
import scalafx.scene.control.TextField
import scalafx.scene.control.Label
import scalafx.scene.control.RadioButton
import scalafx.scene.control.ToggleGroup
import scalafx.scene.control.CheckBox

import scalafx.geometry.Insets
import scalafx.geometry.Orientation

import scalafx.application.JFXApp
import scalafx.application.Platform

import scalafx.collections.ObservableBuffer

import javafx.scene.{layout => jfxsl}
import javafx.scene.{control => jfxsc}

import javafx.beans.property.SimpleStringProperty
import javafx.beans.property.ReadOnlyStringProperty

import orchard.core._
import Util._

class EditorUI extends DialogStack(new StackPane) with EventReactor[CellEvent] {

  //============================================================================================
  // UI INITIALIZATION
  //

  getStyleClass.add("orchard-editor")

  val mainPane = new StackPane(root.asInstanceOf[jfxsl.StackPane])

  val accordionPane = 
    new StackPane {
      padding = Insets(10, 10, 10, 10)
      style = "-fx-background-color: gainsboro"
      content = new Accordion {
        panes = List(
          new TitledPane {
            text = "File"
            content = new VBox {
              padding = Insets(10,10,10,10)
              content = List(
                new Button("New") { prefWidth = 100 ; onAction = newBuilder },
                new Button("Open") { prefWidth = 100 ; onAction = onOpenAction },
                new Button("Save") { prefWidth = 100 ; onAction = onSaveAction })
            }
          },
          new TitledPane {
            text = "Action"
            content = new VBox {
              padding = Insets(10,10,10,10)
              content = List(
                new Button("Assume") { prefWidth = 100 },
                new Button("Fill") { prefWidth = 100 },
                new Button("Compose") { prefWidth = 100 ; onAction = activeBuilder.emptyComposition },
                new Button("Extend") { prefWidth = 100 ; onAction = activeBuilder.complex.extend },
                new Button("Drop") { prefWidth = 100 ; onAction = activeBuilder.emptyDrop },
                new Button("Dump") { prefWidth = 100 ; onAction = displayMessage("Dump", "This is the dump button.") },
                new Button("Refresh") { prefWidth = 100 ; onAction = activeBuilder.refreshAll })
            }
          })
      }
    }

  val editorPane = new TabPane

  class ExpressionWrapper(val expr : NCell[Expression]) {

    private val originStr = 
      expr.value match {
        case Variable(_, _) => "Assumed"
        case _ => "Derived"
      }

    private val thinStr =
      if (expr.value.isThin) "yes" else "no"

    val idProperty : ReadOnlyStringProperty = new SimpleStringProperty(this, "id", expr.value.id)
    val originProperty : ReadOnlyStringProperty = new SimpleStringProperty(this, "origin", originStr)
    val thinProperty : ReadOnlyStringProperty = new SimpleStringProperty(this, "thin", thinStr)

  }

  val environment = new ObservableBuffer[ExpressionWrapper]

  def envContains(id : String) : Boolean = {
    environment exists (expr => expr.idProperty.getValue == id)
  }

  val environmentTable = buildEnvironmentTable

  AnchorPane.setLeftAnchor(environmentTable, 10)
  AnchorPane.setTopAnchor(environmentTable, 10)
  AnchorPane.setBottomAnchor(environmentTable, 10)


  val explorerPane = new AnchorPane {
    content = environmentTable
    style = "-fx-background-color: gainsboro"
  }

  val splitPane = new SplitPane {
    orientation = Orientation.VERTICAL
    dividerPositions = 0.5f
  }

  splitPane.getItems.addAll(editorPane, explorerPane)

  mainPane.content = new HBox {
    content = List(accordionPane, splitPane)
  }

  HBox.setHgrow(splitPane, Priority.ALWAYS)

  newBuilder

  def buildEnvironmentTable : TableView[ExpressionWrapper] = {

    val table = new TableView[ExpressionWrapper] {
      items = environment
    }

    val idColumn = new TableColumn[ExpressionWrapper, String] { text = "Id" ; prefWidth = 100 }
    idColumn.delegate.setCellValueFactory(
      new jfxsc.cell.PropertyValueFactory[ExpressionWrapper, String]("id"))

    val originColumn = new TableColumn[ExpressionWrapper, String] { text = "Origin" }
    originColumn.delegate.setCellValueFactory(
      new jfxsc.cell.PropertyValueFactory[ExpressionWrapper, String]("origin"))

    val thinColumn = new TableColumn[ExpressionWrapper, String] { text = "Thin" }
    thinColumn.delegate.setCellValueFactory(
      new jfxsc.cell.PropertyValueFactory[ExpressionWrapper, String]("thin"))

    table.columns.addAll(idColumn, originColumn, thinColumn)

    table
  }

  //============================================================================================
  // DIALOG DEFINITIONS
  //

  object EditorMessage extends Dialog {

    val message = new Label

    borderPane.center = new StackPane { padding = Insets(10,10,10,10) ; content = message }

    def setTitle(title : String) = heading.text = title
    def setMessage(msg : String) = message.text = msg

    def onHide = ()
    def onShow = ()

  }

  def displayMessage(title : String, message : String) = {
    EditorMessage.setTitle(title)
    EditorMessage.setMessage(message)
    EditorMessage.run
  }

  class FillDialog(emptyCell : ExpressionBuilder#GalleryCell) extends CancellableDialog {

    heading.text = "Fill"

    val assBtn = new RadioButton("New Assumption") 
    val envBtn = new RadioButton("From Environment") 
    val nookBtn = new RadioButton("Fill Exposed Nook")

    val idField = new TextField { promptText = "Identifier" ; onMouseClicked = { assBtn.fire } }
    val compField = new TextField { promptText = "Composite" ; onMouseClicked = { nookBtn.fire } }
    val thinField = new TextField { promptText = "Thin Filler" ; onMouseClicked = { nookBtn.fire } }

    val thinCheckBox = new CheckBox("Thin") { allowIndeterminate = false }

    val envTable = buildEnvironmentTable
    envTable.onMouseClicked = { envBtn.fire }
    envTable.prefHeight = 200

    val sourceToggle = new ToggleGroup {
      toggles = List(assBtn, envBtn, nookBtn)
    }

    assBtn.selected = true

    borderPane.center =
      new VBox {
        padding = Insets(10,10,10,10)
        spacing = 10
        content = List(assBtn, idField, thinCheckBox, envBtn, envTable, nookBtn, compField, thinField)
      }

    def onShow = { idField.requestFocus }

    def onHide = {
      response match {
        case DialogOK => {
          if (assBtn.selected()) {
            if (emptyCell.owner.isShell) {
              if (envContains(idField.text())) {
                println("Duplicate identifier.")
              } else {
                activeBuilder.deselectAll
                emptyCell.owner.item = Neutral(Some(Variable(idField.text(), thinCheckBox.selected())))

                val exprCell : NCell[Expression] = emptyCell.owner.getSimpleFramework.toCell map (_.force)
                environment += new ExpressionWrapper(exprCell)
              }
            } else {
              println("Cell is not a shell!!!")
            }
          } else if (envBtn.selected()) {
            val exprWrapper = envTable.getSelectionModel.getSelectedItem
            if (exprWrapper != null) {

              println("Seeing if it fits ...")

              // Here is perhaps a better way to do this: implement a zipping operation for
              // cells which returns None when they have a different shape and the whole cell
              // with pairs decorating them when they are the same.

              def comparison(e : Option[Expression])(f : Expression) : Boolean =
                e match {
                  case None => true
                  case Some(g) => g == f  // Umm ... is this okay? Oh boy, here comes the equality ...
                }

              val first : NCell[Option[Expression]] = emptyCell.owner.getSimpleFramework.toCell
              val second : NCell[Expression] = exprWrapper.expr

              val itFits = first.compareWith(second, comparison)

              if (itFits) {

                println("it does!!!")

                activeBuilder.deselectAll
                emptyCell.owner.skeleton.simultaneously(second, (e => (f : Expression) => e.item = Neutral(Some(f))))

                println("Uh .. transfer complete?")
              } else {
                println("nope :(")
              }

            } else {
              println("You didn't select anything!!")
            }
          } else {
            if (emptyCell.owner.isExposedNook) {
              println("Filling nook ...")

              if (envContains(compField.text()) || envContains(thinField.text())) {
                println("Duplicate identifier") 
              } else {

                activeBuilder.deselectAll

                val nook = emptyCell.owner.getSimpleFramework.toCell

                val (targetIsThin, targetCell) =
                  if (emptyCell.owner.isOutNook) {
                    ((true /: (emptyCell.owner.sources.force map (_.isThin))) (_&&_), emptyCell.owner.target.force)
                  } else {
                    (emptyCell.owner.target.force.isThin, emptyCell.owner.emptySources.head)
                  }

                targetCell.item = Neutral(Some(FillerTarget(compField.text(), nook, targetIsThin)))
                val tgtExprCell = targetCell.getSimpleFramework.toCell map (_.force)
                environment += new ExpressionWrapper(tgtExprCell)

                emptyCell.owner.item = Neutral(Some(Filler(thinField.text(), nook)))
                val exprCell = emptyCell.owner.getSimpleFramework.toCell map (_.force)
                environment += new ExpressionWrapper(exprCell)
              }
            } else {
              println("Not and exposed nook.")
            }
          }
        }
        case DialogCancel => ()
      }
    }

  }

  //============================================================================================
  // EVENTS
  //

  def onEventEmitted(ev : CellEvent) = 
    ev match {
      case CellDoubleClicked(c) => {
        val cell = c.asInstanceOf[ExpressionBuilder#GalleryCell]

        if (cell.owner.isEmpty) {
          val fillDialog = new FillDialog(cell)
          fillDialog.run
        } else {
          // Let's open the cell in a new tab here
          val theCell : NCell[Option[Expression]] = cell.owner.getSimpleFramework.toCell
          newBuilder(theCell)
        }
      }

      case _ => ()
    }

  def onOpenAction = {
  }

  def onSaveAction = {
  }

  //============================================================================================
  // EDITOR SEMANTICS
  //

  def newBuilder = {
    val builder = new ExpressionBuilder
    editorPane += new Tab { text = "Untitled" ; content = builder }
    reactTo(builder)
    builder.renderAll
  }

  def newBuilder(seed : NCell[Option[Expression]]) = {
    val builder = new ExpressionBuilder(CardinalComplex(seed))
    val newTab = new Tab { text = "Derived" ; content = builder }
    editorPane += newTab
    editorPane.selectionModel().select(newTab)
    reactTo(builder)
    builder.renderAll
  }

  def activeBuilder : ExpressionBuilder = {
    editorPane.getSelectionModel.selectedItem().content().asInstanceOf[ExpressionBuilder]
  }
}

object Editor extends JFXApp {

  val editorUI = new EditorUI

  val orchardScene = new Scene {
      root = editorUI
    }

  orchardScene.getStylesheets.add("orchard/ui/javafx/OrchardUI.css")

  stage = new JFXApp.PrimaryStage {
      title = "Orchard"
      width = 1500
      height = 800
      scene = orchardScene
    }

}
