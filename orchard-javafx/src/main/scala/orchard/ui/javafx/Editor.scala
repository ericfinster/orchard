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
    if (emptyCell.owner.isObject) { thinCheckBox.selected = true }

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
              activeBuilder.deselectAll
              emptyCell.owner.item = Neutral(Some(Variable(idField.text(), thinCheckBox.selected())))
            } else {
              println("Cell is not a shell!!!")
            }
          } else if (envBtn.selected()) {
            println("Environment not supported yet.")
          } else {
            if (emptyCell.owner.isExposedNook) {
              println("Filling nook ...")
              activeBuilder.deselectAll

              val nook = emptyCell.owner.getSimpleFramework.toCell

              if (emptyCell.owner.isOutNook) {
                val targetIsThin = (true /: (emptyCell.owner.sources.force map (_.isThin))) (_&&_)
                emptyCell.owner.target.force.item = Neutral(Some(FillerTarget(compField.text(), nook, targetIsThin)))
              } else {
                val targetIsThin = emptyCell.owner.target.force.isThin
                emptyCell.owner.emptySources.head.item = Neutral(Some(FillerTarget(compField.text(), nook, targetIsThin)))
              }

              emptyCell.owner.item = Neutral(Some(Filler(thinField.text(), nook)))

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
    editorPane += new Tab { content = builder }
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
