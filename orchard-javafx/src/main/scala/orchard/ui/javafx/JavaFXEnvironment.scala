/**
  * JavaFXEnvironment.scala - UI elements for environments
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.Includes._
import scalafx.scene.control._

import orchard.core.expression._

trait JavaFXEnvironmentModule extends EnvironmentModule { thisSystem : JavaFXTypeCheckerMixin =>

  type EnvironmentEntryType = JavaFXEnvironmentEntry
  type IdentifierType = JavaFXIdentifierEntry
  type GroupType = JavaFXGroupEntry

  abstract class JavaFXEnvironmentEntry extends TreeItem[JavaFXEnvironmentEntry] with EnvironmentEntry {

    def parentGroup : Option[GroupType] = {
      val parentItem = parent()

      if (parentItem == null) None else 
        parentItem.value() match {
          case grp : JavaFXGroupEntry => Some(grp)
          case _ => None
        }
    }

    value = this

    def styleString = "unknown"

  }

  case class JavaFXIdentifierEntry(
    val name : String, 
    val moduleEntry : JavaFXModuleEntry
  ) extends JavaFXEnvironmentEntry with IdentifierEntry

  case class JavaFXGroupEntry(
    val name : String, 
    val moduleEntry : JavaFXModuleEntry, 
    val entries : Seq[JavaFXEnvironmentEntry]
  ) extends JavaFXEnvironmentEntry with GroupEntry 

  //============================================================================================
  // CONSTRUCTORS
  //

  def newIdentifierEntry(name : String, moduleEntry : JavaFXModuleEntry) : JavaFXIdentifierEntry = 
    JavaFXIdentifierEntry(name, moduleEntry)

  def newGroupEntry(name : String, moduleEntry : JavaFXModuleEntry, entries : Seq[JavaFXEnvironmentEntry]) : JavaFXGroupEntry = 
    JavaFXGroupEntry(name, moduleEntry, entries)

}
