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

  val moduleAnchor = new AnchorPane {
    content = noModulePane
    styleClass += "orch-pane"
  }

  val moduleBrowserSplit = new SplitPane {
    orientation = Orientation.VERTICAL
    items ++= List(importAnchor, moduleAnchor)
    dividerPositions = 0.3f
  }

  val workspacePane = new StackPane {
    padding = Insets(10, 10, 10, 10)
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

}
