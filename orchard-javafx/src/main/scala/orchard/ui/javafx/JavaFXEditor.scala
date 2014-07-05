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

import orchard.core.expression.Editor
import orchard.core.expression.CheckerResult

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

  def withAssumptionInfo[A](
    thinHint : Boolean,
    forceThin : Boolean,
    handler : (String, Boolean) => CheckerResult[A]
  ) : Unit = {

    val varDialog = new VariableDialog(handler)

    varDialog.thinCheckBox.selected = thinHint

    if (forceThin) {
      varDialog.thinCheckBox.selected = true
      varDialog.thinCheckBox.disable = true
    }

    varDialog.run

  }

  def withFillerIdentifier[A](
    handler : String => CheckerResult[A]
  ) : Unit = {
    val idDialog = new SimpleIdentifierDialog(handler)
    idDialog.setHeading("Fill Nook")
    idDialog.run
  }

  //============================================================================================
  // THE TYPE CHECKER
  //

  var currentTypeChecker : Option[JavaFXTypeChecker] = Some(new JavaFXTypeChecker(this, "Prelude"))

  def typeChecker : Option[JavaFXTypeChecker] = currentTypeChecker
  def typeChecker_=(checker : JavaFXTypeChecker) = {
    currentTypeChecker = Some(checker)
    displayTypeChecker
  }

  def displayTypeChecker : Unit = 
    for {
      checker <- typeChecker
    } {
      modulePane.content = checker.moduleView
      environmentPane.content = checker.environmentView
    }

  displayTypeChecker

}




