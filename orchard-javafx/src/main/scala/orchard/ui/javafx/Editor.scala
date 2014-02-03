/**
  * Editor.scala - Main module for JavaFX Orchard Editor
  * 
  * @author Eric Finster
  * @version 0.1
  */

package orchard.ui.javafx

import scalafx.Includes._

import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.stage.FileChooser

import scalafx.scene.layout._
import scalafx.geometry._

import scalafx.scene.control.TextField
import scalafx.scene.control.Button
import scalafx.scene.control.SplitPane
import scalafx.scene.control.ListView
import scalafx.scene.control.TabPane
import scalafx.scene.control.TitledPane
import scalafx.scene.control.CheckBox

import javafx.event.Event
import javafx.event.EventHandler

import javafx.scene.input.KeyCode
import javafx.scene.input.KeyEvent
import javafx.scene.input.MouseEvent

import javafx.scene.{layout => jfxsl}
import javafx.scene.{control => jfxsc}

import orchard.core._
import orchard.ui.javafx.controls._

class Editor extends PopupManager(new VBox) { thisEditor =>

  implicit val pm = thisEditor

  val mainVBox = new VBox(root.delegate.asInstanceOf[jfxsl.VBox])

  val newDefinitionItem = new MenuItem {
    text = "New Definition"
    onAction = onNewDefinition
  }

  val newSheet = new MenuItem {
    text = "New Sheet"
    onAction = onNewSheet
  }

  val fileMenu = new Menu {
    text = "File"
    items ++= List(newDefinitionItem, newSheet)
  }

  val menuBar = new MenuBar {
    menus ++= List(fileMenu)
  }

  val definitionTabPane = new TabPane {
    id = "orch-definition-pane"
    side = Side.TOP
  }

  val definitionList = new ListView[Definition] {
    cellFactory = (_ => new DefinitionCell)
  }

  class DefinitionCell extends jfxsc.ListCell[Definition] {

    getStyleClass add "orch-list-cell"

    override def updateItem(defn : Definition, empty : Boolean) = {
      super.updateItem(defn, empty)
    }

  }

  val definitionBuilderPane = new StackPane {
    content = definitionTabPane
    padding = Insets(10,10,10,10)
    styleClass += "orch-pane"
  }

  val definitionListPane = {
    val defnTitledPane = 
      new TitledPane {
        text = "Definitions"
        content = definitionList
        collapsible = false
        expanded = true
      }

    AnchorPane.setTopAnchor(defnTitledPane, 10)
    AnchorPane.setRightAnchor(defnTitledPane, 10)
    AnchorPane.setBottomAnchor(defnTitledPane, 10)
    AnchorPane.setLeftAnchor(defnTitledPane, 10)

    new AnchorPane {
      content = defnTitledPane
      styleClass += "orch-pane"
    }
  }

  val horizontalSplit = new SplitPane {
    orientation = Orientation.HORIZONTAL
    items.addAll(definitionListPane, definitionBuilderPane)
    dividerPositions = 0.2f
  }

  VBox.setVgrow(horizontalSplit, Priority.ALWAYS)
  mainVBox.content.addAll(menuBar, horizontalSplit)

  //============================================================================================
  // DIALOG DEFINITIONS
  //

  object NewDefinitionDialog extends Dialog {

    heading.text = "New Definition"

    val nameField = new TextField { promptText = "Definition name" }

    borderPane.center = 
      new VBox {
        padding = Insets(10,10,10,10)
        spacing = 10
        content = List(nameField)
      }

    def onShow = ()

    def onHide = {
      newDefinition(nameField.text())
      nameField.clear
    }

  }

  abstract class ComposeInfoDialog extends CancellableDialog {

    val composeField = new TextField { promptText = "Composite" ; onAction = () => { fillerField.requestFocus } }
    val fillerField = new TextField { promptText = "Filler" ; onAction = () => { okBtn.fire } }

    borderPane.center = 
      new VBox {
        padding = Insets(10,10,10,10)
        spacing = 10
        content = List(composeField, fillerField)
      }

  }

  class FillNookDialog(nookCell : ExpressionBuilder#GalleryCell) extends ComposeInfoDialog {

    heading.text = "Fill Nook"

    def onShow = {
      composeField.clear
      fillerField.clear
      composeField.requestFocus
    }

    def onHide =
      response match {
        case DialogOK => 
          for {
            defnBuilder <- definitionBuilder
            exprBuilder <- expressionBuilder
          } {
            val compositeId = composeField.text()
            val fillerId = fillerField.text()

            if (defnBuilder.envContains(compositeId) || defnBuilder.envContains(fillerId)) {
              println("Error: Duplicate Identifier")
            } else {
              defnBuilder.fillExposedNook(nookCell, composeField.text(), fillerField.text())
            }
        }
        case DialogCancel => ()
      }
  }

  class IdentityDialog(expr : Expression) extends ComposeInfoDialog {

    heading.text = "Insert Identity"

    composeField.text = "id-" ++ expr.id
    fillerField.text = "def-id-" ++ expr.id

    def onShow = composeField.requestFocus

    def onHide =
      response match {
        case DialogOK => 
          for { 
            defnBuilder <- definitionBuilder
            exprBuilder <- expressionBuilder 
          } {
            exprBuilder.extrudeDrop

            val compositeId = composeField.text()
            val fillerId = fillerField.text()

            if (defnBuilder.envContains(compositeId) || defnBuilder.envContains(fillerId)) {
              println("Error: Duplicate Identifier")
            } else {
              defnBuilder.fillExposedNook(exprBuilder.lastFiller, composeField.text(), fillerField.text())
            }
          }
        case DialogCancel => ()
      }

  }

  object ComposeDialog extends ComposeInfoDialog {

    heading.text = "Insert Composite"

    def onShow = {
      composeField.clear()
      fillerField.clear()
      composeField.requestFocus
    }

    def onHide = 
      response match {
        case DialogOK => 
          for {
            defnBuilder <- definitionBuilder
            exprBuilder <- expressionBuilder
          } {
            exprBuilder.extrudeSelection

            val compositeId = composeField.text()
            val fillerId = fillerField.text()

            if (defnBuilder.envContains(compositeId) || defnBuilder.envContains(fillerId)) {
              println("Error: Duplicate Identifier")
            } else {
              defnBuilder.fillExposedNook(exprBuilder.lastFiller, composeField.text(), fillerField.text())
            }
        }
        case DialogCancel => ()
      }
  }

  object VariableDialog extends Dialog {

    heading.text = "Assume Variable"

    val idField = new TextField { promptText = "Identifier" ; onAction = () => { okBtn.fire } }
    val thinCheckBox = new CheckBox("Thin") { allowIndeterminate = false }

    borderPane.center = 
      new VBox {
        padding = Insets(10,10,10,10)
        spacing = 10
        content = List(idField, thinCheckBox)
      }

    def onShow = {
      idField.clear
      idField.requestFocus
    }

    def onHide =
      response match {
        case DialogOK => {
          for {
            defnBuilder <- definitionBuilder
            exprBuilder <- defnBuilder.activeBuilder
          } {
            if (defnBuilder.envContains(idField.text())) {
              println("Error: Duplicate Identifier")
            } else {
              defnBuilder.assume(idField.text(), thinCheckBox.selected())
            }
          }
        }
        case DialogCancel => ()
      }

  }

  //============================================================================================
  // Events
  //

  addEventFilter(KeyEvent.KEY_PRESSED,
    new EventHandler[KeyEvent] {
      def handle(ev : KeyEvent) {
        ev.getCode match {
          case KeyCode.LEFT => for { exprBuilder <- expressionBuilder } exprBuilder.prev 
          case KeyCode.RIGHT => for { exprBuilder <- expressionBuilder } exprBuilder.next
          case KeyCode.E => if (ev.isControlDown) onExtrude
          case KeyCode.D => if (ev.isControlDown) onDrop
          case KeyCode.A => if (ev.isControlDown) onAssume(ev.isShiftDown)
          case KeyCode.C => if (ev.isControlDown) onCompose
          case KeyCode.I => if (ev.isControlDown) onInsertIdentity
          case KeyCode.F => if (ev.isControlDown) onFill  
          case KeyCode.U => if (ev.isControlDown) onUseEnvironment
          // case KeyCode.V => if (ev.isControlDown) onView
          // case KeyCode.O => if (ev.isControlDown) onOpen
          // case KeyCode.S => if (ev.isControlDown) onSave
          case KeyCode.N => if (ev.isControlDown) onNewSheet
          // case KeyCode.L => if (ev.isControlDown) onLoadExpr
          // case KeyCode.G => if (ev.isControlDown) onGlobCardinal
          // case KeyCode.X => if (ev.isControlDown) onExtra
          // case KeyCode.P => if (ev.isControlDown) onPrintScreen
          // case KeyCode.W => if (ev.isControlDown) onWebView
          // case KeyCode.M => if (ev.isControlDown) displayMessage("Message", "This is a message!")
          // case KeyCode.Z => if (ev.isControlDown) { debug = ! debug ; println("Debug is now: " ++ (if (debug) "on" else "off")) }
          case _ => ()
        }
      }
    })

  def onAssume(thin : Boolean) = {
    for { 
      exprBuilder <- expressionBuilder
      cell <- exprBuilder.selectionBase
    } {
      if (cell.owner.isShell) {
        VariableDialog.thinCheckBox.selected = thin
        VariableDialog.run
      } else {
        println("Error: selection is not a shell!")
      }
    }
  }

  def onExtrude = {
    for { exprBuilder <- expressionBuilder } {
      exprBuilder.extrudeSelection
    }
  }

  def onDrop = {
    for { exprBuilder <- expressionBuilder } {
      exprBuilder.extrudeDrop
    }
  }

  def onCompose = {
    for { exprBuilder <- expressionBuilder } {
      if (exprBuilder.selectionIsComposable) ComposeDialog.run
    }
  }

  def onInsertIdentity = {
    for { exprBuilder <- expressionBuilder } {
      if (exprBuilder.selectionIsComposable) {
        for { cell <- exprBuilder.selectionBase } {
          cell.item match {
            case Neutral(Some(expr)) => {
              val idDialog = new IdentityDialog(expr)
              idDialog.run
            }
            case _ => ()
          }
        }
      }
    }
  }

  def onUseEnvironment = {
    for { 
      defnBuilder <- definitionBuilder
      exprBuilder <- expressionBuilder
      selectedCell <- exprBuilder.selectionBase
    } {
      if (selectedCell.owner.isEmpty) {
        val selectedExpr = defnBuilder.envListView.getSelectionModel.getSelectedItem

        if (selectedExpr != null) {
          defnBuilder.fillFromEnvironment(selectedCell, selectedExpr)
        }
      }
    }
  }

  def onFill = {
    for {
      exprBuilder <- expressionBuilder
      selectedCell <- exprBuilder.selectionBase
    } {
      if (selectedCell.owner.isExposedNook) {
        val fillDialog = new FillNookDialog(selectedCell)
        fillDialog.run
      }
    }
  }
  
  def onNewDefinition = {
    NewDefinitionDialog.run
  }

  def onNewSheet = {
    for { defnBldr <- definitionBuilder } { 
      defnBldr.newSheet 
    }
  }

  def newDefinition(name : String) = {
    val builder = new JavaFXDefinitionBuilder
    builder.text = name
    definitionTabPane.tabs += builder
  }

  def definitionBuilder : Option[JavaFXDefinitionBuilder] = {
    val activeDef = 
      definitionTabPane.getSelectionModel.
        selectedItem().asInstanceOf[JavaFXDefinitionBuilder]

    if (activeDef != null) Some(activeDef) else None
  }

  def expressionBuilder : Option[ExpressionBuilder] = {
    for {
      defnBuilder <- definitionBuilder
      exprBuilder <- defnBuilder.activeBuilder
    } yield exprBuilder
  }

  newDefinition("Definition")
}

object Editor extends JFXApp {

  val orchardScene = new Scene(new Editor, 1600, 900)

  orchardScene.getStylesheets.add("orchard/ui/javafx/OrchardUI.css")

  stage = new JFXApp.PrimaryStage {
      title = "Orchard"
      scene = orchardScene
    }

}
