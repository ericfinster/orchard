/**
  * JavaFXWorkspace.scala - UI elements for workspaces
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import orchard.core.expression._

import JavaFXModuleSystem._

class JavaFXWorkspace(module : JavaFXModule) extends Workspace(module) {

  type EditorType = JavaFXEditor
  def editor = OrchardEditor

  def activeWorksheet: Option[Worksheet] = ???

  def invertibilityLevel: Option[Int] = None
  def stabilityLevel: Option[Int] = None
  def unicityLevel: Option[Int] = None

}
