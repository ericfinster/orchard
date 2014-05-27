/**
  * JavaFXEditor.scala - The editor trait
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.Includes._

import orchard.ui.javafx.controls.PopupManager

trait JavaFXEditor {

  implicit def pm : PopupManager

  def consoleMessage(str : String) : Unit
  def consoleError(str : String) : Unit

  def onExit = scalafx.application.Platform.exit

}



