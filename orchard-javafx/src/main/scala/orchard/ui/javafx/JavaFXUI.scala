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

  val upperStack = new StackPane

  val console = new TextArea {
    editable = false
  }

  def consoleMessage(str : String) : Unit = 
    console.text = console.text() ++ "INFO: " ++ str ++ "\n"

  def consoleError(str : String) : Unit = 
    console.text = console.text() ++ "ERROR: " ++ str ++ "\n"

  val verticalSplit = new SplitPane {
    orientation = Orientation.VERTICAL
    items ++= List(upperStack, console)
  }

  val ui = verticalSplit

}
