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

import controls._

abstract class JavaFXEditor extends PopupManager(new VBox) 
    with JavaFXEvents 
    with JavaFXDialogs
    with JavaFXUI
    with JavaFXMenus {

  implicit def pm : PopupManager = this

  //============================================================================================
  // CONSOLE ROUTINES
  //

  def consoleWrite(str : String) : Unit = 
    console.text = console.text() ++ str ++ "\n"

  def consoleMessage(str : String) : Unit = 
    console.text = console.text() ++ "INFO: " ++ str ++ "\n"

  def consoleError(str : String) : Unit = 
    console.text = console.text() ++ "ERROR: " ++ str ++ "\n"


  //============================================================================================
  // MODULE HANDLING
  //

  private var currentModule : Option[JavaFXModule] = None

  def activeModule : Option[JavaFXModule] = currentModule
  def activeModule_=(mod : JavaFXModule) = {
    currentModule = Some(mod)
    moduleAnchor.content = mod.modulePane
    parameterAnchor.content = mod.parameterPane
    workspacePane.content = mod.worksheetTabPane
    consoleMessage("Set active module to: " ++ mod.name)
  }

}




