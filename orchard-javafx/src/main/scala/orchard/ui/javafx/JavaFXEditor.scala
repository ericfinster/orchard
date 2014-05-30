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

import orchard.core.expression.Editor

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
    console.text = console.text() ++ str ++ "\n"

  def consoleMessage(str : String) : Unit = 
    console.text = console.text() ++ "INFO: " ++ str ++ "\n"

  def consoleError(str : String) : Unit = 
    console.text = console.text() ++ "ERROR: " ++ str ++ "\n"

  //============================================================================================
  // IO CALLBACK ROUTINES
  //


  def withAssumptionInfo(thinHint : Boolean,
                         forceThin : Boolean,
                         handler : (String, Boolean) => Unit) : Unit = {

    val varDialog = new VariableDialog(handler)

    varDialog.thinCheckBox.selected = thinHint

    if (forceThin) {
      varDialog.thinCheckBox.selected = true
      varDialog.thinCheckBox.disable = true
    }

    varDialog.run
  }


  //============================================================================================
  // MODULE HANDLING
  //

  private var myTopLevelModule : Option[JavaFXModule] = None

  def topLevelModule : Option[JavaFXModule] = myTopLevelModule
  def topLevelModule_=(mod : JavaFXModule) = {
    myTopLevelModule = Some(mod)
    moduleView.root = mod.treeItem
    moduleAnchor.content = modulePane
    parameterAnchor.content = parameterPane
    moduleView.selectionModel().select(mod.treeItem)
  }

  private var myActiveModule : Option[JavaFXModule] = None

  def activeModule : Option[JavaFXModule] = myActiveModule
  def activeModule_=(mod : JavaFXModule) = {
    myActiveModule = Some(mod)
    workspacePane.content = mod.worksheetTabPane
    consoleMessage("Set active module to: " ++ mod.name)
  }

  def displayParameters(entry : JavaFXModuleEntry) : Unit = {
    parameterView.items().clear
    parameterView.items() ++= (entry.parameters map (_.asInstanceOf[JavaFXModuleVariable]))
  }

  moduleView.selectionModel().selectedItem onChange {
    val item = moduleView.selectionModel().selectedItem()

    if (item != null) {
      val entry = item.value()
      displayParameters(entry)

      item.value() match {
        case mod : JavaFXModule => activeModule = mod
        case _ => ()
      }

    }
  }

}


