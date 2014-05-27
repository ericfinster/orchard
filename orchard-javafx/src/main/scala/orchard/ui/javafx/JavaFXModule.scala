/**
  * JavaFXModule.scala - JavaFX Implementation of a Module
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scala.collection.mutable.Buffer

import scalafx.Includes._
import scalafx.geometry._
import scalafx.scene.layout._
import scalafx.scene.control._

import orchard.core.expression._

class JavaFXModule(
  val name : String,
  val stabilityLevel : Option[Int], 
  val invertibilityLevel : Option[Int],
  val unicityLevel : Option[Int]
) extends JavaFXScope with Module {

  type ModuleType = JavaFXModule
  type SectionEntryType = JavaFXSectionEntry

  def imports : Buffer[JavaFXModule] = ??? 
  def entries : Buffer[JavaFXSectionEntry] = ???

  //============================================================================================
  // UI
  //

  val moduleTreeView = new TreeView
  val importTreeView = new TreeView

  val modulePane = new StackPane {
    padding = Insets(10, 10, 10, 10)
    content = new TitledPane {
      text = name
      content = moduleTreeView
      collapsible = false
    }

    styleClass += "orch-pane"
  }

  val importsPane = new StackPane {
    padding = Insets(10, 10, 10, 10)
    content = new TitledPane {
      text = "Imports"
      content = importTreeView
      collapsible = false
    }

    styleClass += "orch-pane"
  }

  val workspacePane = new StackPane {
    styleClass += "orch-pane"
  }

  val leftVerticalSplit = new SplitPane {
    orientation = Orientation.VERTICAL
    items ++= List(modulePane, importsPane)
  }

  val horizontalSplit = new SplitPane {
    orientation = Orientation.HORIZONTAL
    items ++= List(leftVerticalSplit, workspacePane)
    dividerPositions = 0.1f
  }

  val ui = horizontalSplit

}

