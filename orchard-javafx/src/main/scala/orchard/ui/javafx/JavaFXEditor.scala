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
import JavaFXModuleSystem._

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
    console.appendText(str ++ "\n")

  def consoleMessage(str : String) : Unit =  
    console.appendText("INFO: " ++ str ++ "\n")

  def consoleError(str : String) : Unit =
    console.appendText("ERROR: " ++ str ++ "\n")

  def consoleDebug(str : String) : Unit = 
    console.appendText("DEBUG: " ++ str ++ "\n")

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

  def withFillerIdentifier(handler : String => Unit) : Unit = {
    val idDialog = new SimpleIdentifierDialog(handler)
    idDialog.setHeading("Fill Nook")
    idDialog.run
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
    environmentAnchor.content = environmentPane
    moduleView.selectionModel().select(mod.treeItem)
  }

  private var myActiveModule : Option[JavaFXModule] = None

  def activeModule : Option[JavaFXModule] = myActiveModule
  def activeModule_=(modOpt : Option[JavaFXModule]) = {
    if (myActiveModule != modOpt) {
      myActiveModule = modOpt

      for { mod <- modOpt } {
        workspacePane.content = mod.ui
      }
    }
  }

  def displayEnvironment : Unit = {
    environmentRoot.children.clear

    for {
      mod <- activeModule
    } {

      // Problem: cannot use the same tree item in both cases.
      // I think maybe what we should do is just have a method which
      // builds the tree item for an entry instead of having a single one.
      // Hmmm.  Or just have a method to clone it.

      environmentRoot.children = (mod.localEnvironment map (_.cloneTreeItem))
    }
  }

  def activeEntry : Option[JavaFXModuleEntry] = {
    val item = moduleView.selectionModel().selectedItem()

    if (item != null) {
      val entry = item.value()
      if (entry != null) Some(entry) else None
    } else None

  }
}


