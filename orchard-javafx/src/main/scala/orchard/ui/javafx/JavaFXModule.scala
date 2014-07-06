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
  type ScopeType = JavaFXScope
  type ExpressionEntryType = JavaFXExpressionEntry
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

  }

  abstract class JavaFXScope extends JavaFXModuleEntry with Scope
  abstract class JavaFXExpressionEntry extends JavaFXModuleEntry with ExpressionEntry

  case class JavaFXModule(val name : String) extends JavaFXScope with Module {

    def entries : Seq[JavaFXModuleEntry] =
      children map (_.value())

    def appendEntry(entry : JavaFXModuleEntry) : Unit = 
      children += entry

  }

  case class JavaFXImport(override val name : String) extends JavaFXScope with Import {

    def entries : Seq[JavaFXModuleEntry] =
      children map (_.value())

    def isOpen : Boolean = ???

  }

  case class JavaFXParameter(val variable : Variable)
      extends JavaFXExpressionEntry with Parameter {
    def expression = variable
  }

  case class JavaFXLift(val filler : Filler) extends JavaFXExpressionEntry with Lift {

    def fillerEntry = LiftFillerEntry
    def expression = filler.Boundary

    object LiftFillerEntry extends JavaFXExpressionEntry with FillerEntry {
      def expression = filler
    }

    children += LiftFillerEntry

  }

  protected def newModule(name : String) : JavaFXModule = JavaFXModule(name)
  protected def newImport(name : String) : JavaFXImport = JavaFXImport(name)
  protected def newParameter(variable : Variable) : JavaFXParameter = JavaFXParameter(variable)
  protected def newLift(filler : Filler) : JavaFXLift = JavaFXLift(filler)

}
