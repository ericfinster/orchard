/**
  * JavaFXModuleVariable.scala - UI Elements for Variables
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import orchard.core.expression._

class JavaFXModuleVariable(val parent : Option[JavaFXModule], val varExpr : Variable) 
    extends JavaFXModuleEntry 
    with JavaFXModuleEnvironment.ModuleVariable
    with ModuleVariable {

  def liftVariable = this

}
