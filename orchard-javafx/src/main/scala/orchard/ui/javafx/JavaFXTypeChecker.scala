/**
  * JavaFXTypeChecker.scala - Type checker UI elements for JavaFX
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.Includes._
import scalafx.geometry._
import scalafx.scene.layout._
import scalafx.scene.control._

import orchard.core.expression._

// I don't think you reall want this to be an object ...
trait JavaFXTypeCheckerMixin 
    extends TypeChecker
    with JavaFXModuleModule
    with JavaFXEnvironmentModule
    with JavaFXWorkspaceModule
    with CellDefinitions

class JavaFXTypeChecker(val editor : JavaFXEditor, val rootModuleName : String) extends JavaFXTypeCheckerMixin {

  type EditorType = JavaFXEditor

  def rootModule : JavaFXModule = new JavaFXModule(rootModuleName)

  //============================================================================================
  // MODULE MANIPULATION
  //

  var activeModule : Option[JavaFXModule] = None
  var focusedModule : Option[JavaFXModule] = None

  //============================================================================================
  // UI ELEMENTS
  //

  val moduleView = new TreeView[JavaFXModuleEntry] {
    root = rootModule
    cellFactory = (_ => new ModuleTreeCell)
  }

  moduleView.selectionModel().selectedItem onChange {
    val item = moduleView.selectionModel().selectedItem()

    if (item != null) {
      item.value() match {
        case mod : JavaFXModule => focusedModule = Some(mod)
        case _ =>
          item.value().parentScope match {
            case None => focusedModule = activeModule
            case Some(m : JavaFXModule) => focusedModule = Some(m)
            case _ => ???
          }
      }
    }
  }

  val modulePane = new StackPane {
    padding = Insets(10, 10, 10, 10)
    content = moduleView
    styleClass += "orch-pane"
  }

}
