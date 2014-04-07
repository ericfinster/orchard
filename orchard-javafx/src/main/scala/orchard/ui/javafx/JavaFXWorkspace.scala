/**
  * JavaFXWorkspace.scala - JavaFX implementation of a workspace
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import orchard.core.cell._
import orchard.core.editor._
import orchard.core.expression._

class JavaFXWorkspace(
  val editor : JavaFXEditor,
  val name : String,
  val stabilityLevel : Option[Int],
  val invertibilityLevel : Option[Int],
  val unicityLevel : Option[Int]
) extends Workspace {

  var activeSheet : Option[Worksheet] = None
  var activeExpression : Option[NCell[Expression]] = None

  def newSheet = ()

}
