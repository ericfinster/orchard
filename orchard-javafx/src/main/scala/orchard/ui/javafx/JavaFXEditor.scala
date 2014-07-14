/**
  * JavaFXEditor.scala - The editor trait
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.Includes._
import scalafx.scene.Node
import scalafx.scene.layout._
import scalafx.stage.FileChooser

import orchard.core.expression.Editor
import orchard.core.expression.CheckerResult

import controls._

abstract class JavaFXEditor extends PopupManager(new VBox) 
    with JavaFXEvents 
    with JavaFXDialogs
    with JavaFXUI
    with JavaFXMenus {

  implicit def pm : PopupManager = this

  val fileChooser = new FileChooser

  //============================================================================================
  // CONSOLE ROUTINES
  //

  def consoleWrite(str : String) : Unit = 
    console.appendText(str ++ "\n")

  def consoleMessage(str : String) : Unit = 
    console.appendText("INFO: " ++ str ++ "\n")

  def consoleError(str : String) : Unit = 
    console.appendText("ERROR: " ++ str ++ "\n")

  def consoleDebug(str: String): Unit =
    console.appendText("DEBUG: " ++ str ++ "\n")

}




