/**
  * JavaFXSubstitutionWorkspace.scala - JavaFX implementation of a SubstitutionWorkspace
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import orchard.core._

class JavaFXSubstitutionWorkspace(
  val name : String, 
  val parentWorkspace : Workspace,
  val defn : Definition, 
  val shell : SimpleFramework
) extends SubstitutionWorkspace with JavaFXWorkspace {

  def stabilityLevel : Option[Int] = parentWorkspace.stabilityLevel
  def invertibilityLevel : Option[Int] = parentWorkspace.invertibilityLevel
  def unicityLevel : Option[Int] = parentWorkspace.unicityLevel
  
}
