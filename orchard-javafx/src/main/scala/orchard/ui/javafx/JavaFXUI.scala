/**
  * JavaFXUI.scala - JavaFX UI Components
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.Includes._

import scalafx.geometry._
import scalafx.scene.input._
import scalafx.scene.layout._
import scalafx.scene.control._

import scalafx.stage.FileChooser

import JavaFXModuleSystem._

trait JavaFXUI { thisEditor : JavaFXEditor =>

  val fileChooser = new FileChooser

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
    showRoot = true
    cellFactory = (_ => 
      new TreeCell(new ModuleTreeCell) { thisCell =>
        // onMouseClicked = (ev : MouseEvent) => {
        //   if (thisCell.item().isInstanceOf[JavaFXModuleParameter] && ev.clickCount > 1) {
        //     for {
        //       wksp <- activeWorkspace
        //     } {
        //       mod.newSheet(thisCell.item().asInstanceOf[JavaFXModuleParameter].variable)
        //     }
        //   }
        // }
      }
    )
  }

  val modulePane = new TitledPane {
    text = "Current Module"
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

  val workspacePane = new StackPane {
    styleClass += "orch-pane"
  }

  val noEnvironmentPane = new StackPane {
    content = new Label("No Environment")
    styleClass += "orch-pane"
  }

  AnchorPane.setTopAnchor(noEnvironmentPane, 10)
  AnchorPane.setRightAnchor(noEnvironmentPane, 10)
  AnchorPane.setBottomAnchor(noEnvironmentPane, 10)
  AnchorPane.setLeftAnchor(noEnvironmentPane, 10)

  val environmentRoot = new TreeItem[JavaFXModuleEntry]
  val environmentView = new TreeView[JavaFXModuleEntry] {
    cellFactory = (_ => new ModuleTreeCell)
    showRoot = false
    root = environmentRoot
  }

  environmentView.selectionModel().selectedItem onChange {
    val item = environmentView.selectionModel().selectedItem()

    if (item != null) {
      val entry = item.value()

      if (entry != null) {
        entry match {
          case ee : ExpressionEntry => 
            for {
              mod <- activeModule
            } {
              mod.clipboardExpression = Some(ee.expression)
            }
          case _ => ()
        }
      }
    }
  }

  val environmentPane = new TitledPane {
    text = "Environment"
    content = environmentView
    collapsible = false
  }

  AnchorPane.setTopAnchor(environmentPane, 10)
  AnchorPane.setRightAnchor(environmentPane, 10)
  AnchorPane.setBottomAnchor(environmentPane, 10)
  AnchorPane.setLeftAnchor(environmentPane, 10)

  val environmentAnchor = new AnchorPane {
    content = noEnvironmentPane
    styleClass += "orch-pane"
  }

  val horizontalSplit = new SplitPane {
    orientation = Orientation.HORIZONTAL
    items ++= List(moduleBrowserSplit, workspacePane, environmentAnchor)
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
