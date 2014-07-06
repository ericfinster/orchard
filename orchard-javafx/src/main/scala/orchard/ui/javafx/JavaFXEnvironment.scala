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

  }

  case class JavaFXIdentifierEntry(
    val moduleEntry : JavaFXExpressionEntry
  ) extends JavaFXEnvironmentEntry with IdentifierEntry {

    override def toString = name

  }

  case class JavaFXGroupEntry(
    val moduleEntry : JavaFXScope, 
    val entries : Seq[JavaFXEnvironmentEntry]
  ) extends JavaFXEnvironmentEntry with GroupEntry {

    children ++= entries map (_.delegate)

    override def toString = name

  }

  //============================================================================================
  // CONSTRUCTORS
  //

  def newIdentifierEntry(exprEntry : JavaFXExpressionEntry) : JavaFXIdentifierEntry = 
    JavaFXIdentifierEntry(exprEntry)

  def newGroupEntry(scope : JavaFXScope, entries : Seq[JavaFXEnvironmentEntry]) : JavaFXGroupEntry = 
    JavaFXGroupEntry(scope, entries)

}
