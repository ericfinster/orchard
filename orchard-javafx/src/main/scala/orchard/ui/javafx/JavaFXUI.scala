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
import scalafx.scene.text._

trait JavaFXUI { thisEditor : JavaFXEditor =>

  val renderingSurface = new HBox {
    style = "-fx-background-color: white"
  }

  val workspacePane = new StackPane {
    padding = Insets(10, 10, 10, 10)
    content = renderingSurface
    styleClass += "orch-pane"
  }

  val console = new TextArea {
    editable = false
  }

  val verticalSplit = new SplitPane {
    orientation = Orientation.VERTICAL
    items ++= List(workspacePane, console)
    dividerPositions = 0.8f
  }

  val ui = verticalSplit

}
