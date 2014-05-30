/**
  * JavaFXModule.scala - JavaFX Modules
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import orchard.core.expression._

class JavaFXModule extends JavaFXModuleEntry with Module {

  type EntryType = JavaFXModuleEntry

  def entries : Seq[JavaFXModuleEntry] = ???

}
