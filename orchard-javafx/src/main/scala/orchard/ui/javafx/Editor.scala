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

  val fileMenu = new Menu {
    text = "File"
    items ++= List(newDefinitionItem)
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

  // Dialogs

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

  // Event Stuff

  addEventFilter(KeyEvent.KEY_PRESSED,
    new EventHandler[KeyEvent] {
      def handle(ev : KeyEvent) {
        ev.getCode match {
          case _ => ()
        }
      }
    })

  def onNewDefinition = {
    NewDefinitionDialog.run
  }

  def newDefinition(name : String) = {
    val builder = new JavaFXDefinitionBuilder
    builder.text = name
    definitionTabPane.tabs += builder
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
