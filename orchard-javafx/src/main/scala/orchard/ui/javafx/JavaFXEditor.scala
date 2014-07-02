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

import JavaFXModuleSystem._

import orchard.core.expression.Editor

import controls._

abstract class JavaFXEditor extends PopupManager(new VBox) 
    with JavaFXEvents 
    with JavaFXDialogs
    with JavaFXUI
    with JavaFXMenus 
    with Editor {

  implicit def pm : PopupManager = this

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

  //============================================================================================
  // INPUT CALLBACKS
  //

  def withAssumptionInfo(thinHint : Boolean, forceThin : Boolean, handler : (String, Boolean) => Unit) : Unit = ???
  def withFillerIdentifier(handler : String => Unit) : Unit = ???

  //============================================================================================
  // MODULE MANIPULATION
  //

  var activeModule : Option[JavaFXModule] = None
  var focusedModule : Option[JavaFXModule] = None

  def refreshModuleView(mod : Module) : Unit = ()

}




