/**
  * JavaFXDefinitionWorkspace.scala - JavaFX implementation of a DefinitionWorkspace
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import orchard.core._

class JavaFXDefinitionWorkspace(
  val name : String,
  val stabilityLevel : Option[Int],
  val invertibilityLevel : Option[Int],
  val unicityLevel : Option[Int]
) extends DefinitionWorkspace with JavaFXWorkspace {


}

