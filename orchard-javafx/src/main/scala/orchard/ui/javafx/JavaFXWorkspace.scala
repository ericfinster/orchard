/**
  * JavaFXWorkspace.scala - JavaFX implementation of a workspace
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

// import scalafx.scene.text.Text
// import scalafx.scene.layout.Region

import orchard.core.ui._
import orchard.core.cell._
import orchard.core.editor._
import orchard.core.complex._
import orchard.core.expression._

// import javafx.{scene => jfxs}

class JavaFXWorkspace(
  val editor : JavaFXEditor,
  val name : String,
  val stabilityLevel : Option[Int],
  val invertibilityLevel : Option[Int],
  val unicityLevel : Option[Int]
) extends Workspace with JavaFXWorksheetEnv {

  var activeSheet : Option[Worksheet] = None
  var activeExpression : Option[NCell[Expression]] = None

  def newSheet = ()

}
