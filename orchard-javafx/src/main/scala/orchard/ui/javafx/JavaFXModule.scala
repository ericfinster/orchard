/**
  * JavaFXModule.scala - UI Elements for the module system
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.Includes._
import scalafx.scene.control._

import orchard.core.expression._

trait JavaFXModuleModule extends ModuleModule { thisSystem : JavaFXTypeCheckerMixin =>

  type ModuleEntryType = JavaFXModuleEntry
  type ModuleType = JavaFXModule
  type ParameterType = JavaFXParameter
  type LiftType = JavaFXLift
  type ImportType = JavaFXImport

  abstract class JavaFXModuleEntry extends TreeItem[JavaFXModuleEntry] with ModuleEntry {

    def parentScope : Option[Scope] = {
      val parentItem = parent()

      if (parentItem == null) None else 
        parentItem.value() match {
          case mod : JavaFXModule => Some(mod)
          case imprt : JavaFXImport => Some(imprt)
          case _ => None
        }
    }

    value = this

    def styleString = "unknown"
    def name = "unknown"

  }

  case class JavaFXModule(override val name : String) extends JavaFXModuleEntry with Module {

    def entries : Seq[JavaFXModuleEntry] =
      children map (_.value())

    def appendEntry(entry : JavaFXModuleEntry) : Unit = 
      children += entry

  }

  case class JavaFXParameter(val ident : Identifier, val shell : Shell, val isThin : Boolean) 
      extends JavaFXModuleEntry with Parameter

  case class JavaFXLift(filler : Filler) extends JavaFXModuleEntry with Lift

  case class JavaFXImport(override val name : String) extends JavaFXModuleEntry with Import {

    def entries : Seq[JavaFXModuleEntry] =
      children map (_.value())

    def isOpen : Boolean = ???

  }

  protected def newModule(name : String) : JavaFXModule = JavaFXModule(name)
  protected def newImport(name : String) : JavaFXImport = JavaFXImport(name)
  protected def newParameter(ident : Identifier, shell : Shell, isThin : Boolean) : JavaFXParameter = 
    JavaFXParameter(ident, shell, isThin)
  protected def newLift(filler : Filler) : JavaFXLift = JavaFXLift(filler)


}
