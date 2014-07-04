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

  val noWorkspaceLabel = new Label("No Workspace")

  val modulePane = new StackPane {
    padding = Insets(10, 10, 10, 10)
    styleClass += "orch-pane"
  }

  val workspacePane = new StackPane {
    padding = Insets(10, 10, 10, 10)
    content = noWorkspaceLabel
    styleClass += "orch-pane"
  }

  val environmentPane = new StackPane {
    padding = Insets(10, 10, 10, 10)
    styleClass += "orch-pane"
  }

  val horizontalSplit = new SplitPane {
    orientation = Orientation.HORIZONTAL
    items ++= List(modulePane, workspacePane, environmentPane)
  }

  horizontalSplit.setDividerPositions(0.2f, 0.8f)

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
