/**
  * JavaFXTypeChecker.scala - Type checker UI elements for JavaFX
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.Includes._
import scalafx.scene.control._

import orchard.core.expression._

// I don't think you reall want this to be an object ...
trait JavaFXTypeCheckerMixin 
    extends TypeChecker
    with JavaFXModuleModule
    with JavaFXEnvironmentModule

object JavaFXTypeChecker extends JavaFXTypeCheckerMixin {

  type EditorType = JavaFXEditor
  def editor = OrchardEditor

  def rootModule : CheckerResult[Module] = ???

}
