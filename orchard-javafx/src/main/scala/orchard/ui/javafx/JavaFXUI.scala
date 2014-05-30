/**
  * JavaFXUI.scala - JavaFX UI Components
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.Includes._

import scalafx.geometry._
import scalafx.scene.layout._
import scalafx.scene.control._

import javafx.scene.{control => jfxsc}

trait JavaFXUI { thisEditor : JavaFXEditor =>

  val importRoot = new TreeItem[JavaFXModuleEntry]
  val importView = new TreeView[JavaFXModuleEntry] {
    root = importRoot
    showRoot = false
    cellFactory = (_ => new ModuleTreeCell)
  }

  val importPane = new TitledPane {
    text = "Imported Modules"
    collapsible = false
    content = importView
  }

  AnchorPane.setTopAnchor(importPane, 10)
  AnchorPane.setRightAnchor(importPane, 10)
  AnchorPane.setBottomAnchor(importPane, 10)
  AnchorPane.setLeftAnchor(importPane, 10)

  val importAnchor = new AnchorPane {
    content = importPane
    styleClass += "orch-pane"
  }

  val noModulePane = new StackPane {
    content = new Label("No Module Loaded")
    styleClass += "orch-pane"
  }

  AnchorPane.setTopAnchor(noModulePane, 10)
  AnchorPane.setRightAnchor(noModulePane, 10)
  AnchorPane.setBottomAnchor(noModulePane, 10)
  AnchorPane.setLeftAnchor(noModulePane, 10)

  val moduleView = new TreeView[JavaFXModuleEntry] {
    showRoot = false
    cellFactory = (_ => new ModuleTreeCell)
  }

  val modulePane = new TitledPane {
      text = "Untitled"
      collapsible = false
      content = moduleView
    }

  AnchorPane.setTopAnchor(modulePane, 10)
  AnchorPane.setRightAnchor(modulePane, 10)
  AnchorPane.setBottomAnchor(modulePane, 10)
  AnchorPane.setLeftAnchor(modulePane, 10)

  val moduleAnchor = new AnchorPane {
    content = noModulePane
    styleClass += "orch-pane"
  }

  val moduleBrowserSplit = new SplitPane {
    orientation = Orientation.VERTICAL
    items ++= List(importAnchor, moduleAnchor)
    dividerPositions = 0.3f
  }

  val workspacePane = new TabPane {
    styleClass += "orch-pane"
  }

  val noParametersPane = new StackPane {
    content = new Label("No Parameters")
    styleClass += "orch-pane"
  }

  AnchorPane.setTopAnchor(noParametersPane, 10)
  AnchorPane.setRightAnchor(noParametersPane, 10)
  AnchorPane.setBottomAnchor(noParametersPane, 10)
  AnchorPane.setLeftAnchor(noParametersPane, 10)

  val parameterView = new ListView[JavaFXModuleEntry] {
    cellFactory = (_ => new ModuleListCell)
  }

  val parameterPane = new TitledPane {
    text = "Parameters"
    content = parameterView
    collapsible = false
  }

  AnchorPane.setTopAnchor(parameterPane, 10)
  AnchorPane.setRightAnchor(parameterPane, 10)
  AnchorPane.setBottomAnchor(parameterPane, 10)
  AnchorPane.setLeftAnchor(parameterPane, 10)

  val parameterAnchor = new AnchorPane {
    content = noParametersPane
    styleClass += "orch-pane"
  }

  val horizontalSplit = new SplitPane {
    orientation = Orientation.HORIZONTAL
    items ++= List(moduleBrowserSplit, workspacePane, parameterAnchor)
  }

  horizontalSplit.setDividerPositions(0.1f, 0.8f)

  val console = new TextArea {
    editable = false
  }

  val verticalSplit = new SplitPane {
    orientation = Orientation.VERTICAL
    items ++= List(horizontalSplit, console)
    dividerPositions = 0.8f
  }

  val ui = verticalSplit

  //============================================================================================
  // CELL IMPLEMENTATIONS
  //

  class ModuleTreeCell extends jfxsc.TreeCell[JavaFXModuleEntry] {

    getStyleClass add "orch-list-cell"

    override def updateItem(item : JavaFXModuleEntry, empty : Boolean) = {
      if (! empty) {
        setText(item.name)
      }
    }

  }

  class ModuleListCell extends jfxsc.ListCell[JavaFXModuleEntry] {

    getStyleClass add "orch-list-cell"

    override def updateItem(item : JavaFXModuleEntry, empty : Boolean) = {
      if (! empty) {
        setText(item.name)
      }
    }

  }

}
