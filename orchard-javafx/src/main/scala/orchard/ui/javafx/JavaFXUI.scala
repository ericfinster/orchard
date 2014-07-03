/**
  * JavaFXUI.scala - JavaFX UI Components
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.Includes._

import scalafx.geometry._
import scalafx.scene.layout._
import scalafx.scene.control._

import JavaFXTypeChecker._

trait JavaFXUI { thisEditor : JavaFXEditor =>

  val nothing = new Label("Nothing")

  val secondPane = new StackPane {
    padding = Insets(10, 10, 10, 10)
    content = nothing
    styleClass += "orch-pane"
  }

  // This construction should go elsewhere ...
  val moduleRoot = new JavaFXModule("Untitled")
  val moduleView = new TreeView[JavaFXModuleEntry] {
    root = moduleRoot
    cellFactory = (_ => new ModuleTreeCell)
  }

  moduleView.selectionModel().selectedItem onChange {
    val item = moduleView.selectionModel().selectedItem()

    if (item != null) {
      // item.value() match {
      //   case mod : JavaFXModule => focusedModule = Some(mod)
      //   case _ => 
      //     item.value().parentModule match {
      //       case None => focusedModule = activeModule
      //       case Some(m) => focusedModule = Some(m)
      //     }
      // }
    }
  }

  val modulePane = new StackPane {
    padding = Insets(10, 10, 10, 10)
    content = moduleView
    styleClass += "orch-pane"
  }

  val horizontalSplit = new SplitPane {
    orientation = Orientation.HORIZONTAL
    items ++= List(modulePane, secondPane)
    dividerPositions = 0.2f
  }

  val console = new TextArea {
    editable = false
  }

  val verticalSplit = new SplitPane {
    orientation = Orientation.VERTICAL
    items ++= List(horizontalSplit, console)
    dividerPositions = 0.8f
  }

  val ui = verticalSplit

}
