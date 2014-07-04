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

  var myFocusedModule : Option[JavaFXModule] = None

  def focusedModule : Option[JavaFXModule] = myFocusedModule
  def focusedModule_=(modOpt : Option[JavaFXModule]) = {
    myFocusedModule = modOpt

    for {
      mod <- modOpt
    } {
      val environmentRoot = new TreeItem[JavaFXEnvironmentEntry] {
        children ++= (getEnvironment(mod) map (_.delegate))
      }

      environmentView.root = environmentRoot
    }
  }

  //============================================================================================
  // WORKSPACE MANIPULATION
  //

  var myActiveWorkspace : Option[JavaFXWorkspace] = None

  def activeWorkspace : Option[JavaFXWorkspace] = myActiveWorkspace
  def activeWorkspace_=(wkspOpt : Option[JavaFXWorkspace]) : Unit = {
    myActiveWorkspace = wkspOpt

    // Now display it ...
    displayWorkspace
  }

  def displayWorkspace : Unit = 
    for {
      wksp <- activeWorkspace
    } {
      editor.workspacePane.content = wksp.ui
    }

  def newWorkspace : Unit = 
    for {
      mod <- focusedModule
    } {
      val wksp = new JavaFXWorkspace(mod)
      activeWorkspace = Some(wksp)
      wksp.newSheet
    }

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


  val environmentView = new TreeView[JavaFXEnvironmentEntry] {
    showRoot = false
    cellFactory = (_ => new EnvironmentTreeCell)
  }

}
