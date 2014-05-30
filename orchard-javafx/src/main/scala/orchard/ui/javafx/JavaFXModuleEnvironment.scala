/**
  * JavaFXModuleEnvironment.scala - The Module Environment for JavaFX
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import orchard.core.expression._

object JavaFXModuleEnvironment extends ModuleEnvironment {

  type EditorType = JavaFXEditor
  type EntryType = JavaFXModuleEntry
  type ModuleType = JavaFXModule
  type VariableType = JavaFXModuleVariable

}
