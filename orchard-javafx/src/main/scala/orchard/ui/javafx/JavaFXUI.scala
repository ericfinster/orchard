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

  val noModuleLabel = new Label("No Module Loaded")

  val moduleDisplayPane = new StackPane {
    content = noModuleLabel
    styleClass += "orch-pane"
  }

  val console = new TextArea {
    editable = false
  }

  val verticalSplit = new SplitPane {
    orientation = Orientation.VERTICAL
    items ++= List(moduleDisplayPane, console)
    dividerPositions = 0.8f
  }

  val ui = verticalSplit

}
