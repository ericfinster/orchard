/**
  * JavaFXModule.scala - JavaFX Implementation of a Module
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.Includes._
import scalafx.scene.layout._
import scalafx.scene.control._

import orchard.core.editor._

class JavaFXModule(
  val name : String,
  val stabilityLevel : Option[Int], 
  val invertibilityLevel : Option[Int],
  val unicityLevel : Option[Int]
) extends Module {

  def imports : Seq[Module] = ??? 
  def entries : Seq[SectionEntry] = ???

  //============================================================================================
  // UI
  //

  val moduleTreeView = new TreeView
  val importTreeView = new TreeView

  val modulePane = new TitledPane {
    text = name
    content = moduleTreeView
  }

  val importsPane = new TitledPane {
    text = "Imports"
    content = importTreeView
  }

  val workspacePane = new StackPane {
    styleClass += "orch-pane"
  }

  val leftVerticalSplit = new SplitPane {
    items ++= List(modulePane, importsPane)
  }

  val horizontalSplit = new SplitPane {
    items ++= List(leftVerticalSplit, workspacePane)
  }

  val ui = horizontalSplit

}

