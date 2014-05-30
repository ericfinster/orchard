/**
  * JavaFXModuleEntry.scala - Abstract module entries
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.Includes._
import scalafx.scene.control._

import orchard.core.expression._

abstract class JavaFXModuleEntry extends ModuleEntry with JavaFXModuleEnvironment.ModuleEntry {

  def liftEntry = this

  val treeItem = new TreeItem[JavaFXModuleEntry](this)

}
