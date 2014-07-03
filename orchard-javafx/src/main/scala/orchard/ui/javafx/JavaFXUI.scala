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

  val nothing = new Label("Nothing")

  val secondPane = new StackPane {
    padding = Insets(10, 10, 10, 10)
    content = nothing
    styleClass += "orch-pane"
  }

  val horizontalSplit = new SplitPane {
    orientation = Orientation.HORIZONTAL
    dividerPositions = 0.2f
  }

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
