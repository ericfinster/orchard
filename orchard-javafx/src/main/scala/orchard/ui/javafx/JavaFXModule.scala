/**
  * JavaFXModule.scala - UI Elements for the module system
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.Includes._
import scalafx.scene.control._

import scala.collection.JavaConversions._
import javafx.scene.{control => jfxsc}

import orchard.core.expression._


trait JavaFXModuleModule extends ModuleModule { thisSystem : JavaFXTypeCheckerMixin =>

  type ModuleEntryType = JavaFXModuleEntry
  type ScopeType = JavaFXScope
  type ExpressionEntryType = JavaFXExpressionEntry
  type ModuleType = JavaFXModule
  type ParameterType = JavaFXParameter
  type LiftType = JavaFXLift
  type ImportType = JavaFXImport

  abstract class JavaFXModuleEntry extends jfxsc.TreeItem[JavaFXModuleEntry] with ModuleEntry {

    def parentScope : Option[Scope] = {
      val parentItem = getParent

      if (parentItem == null) None else 
        parentItem.getValue match {
          case mod : JavaFXModule => Some(mod)
          case imprt : JavaFXImport => Some(imprt)
          case _ => None
        }
    }

    setValue(this)

  }

  abstract class JavaFXScope extends JavaFXModuleEntry with Scope
  abstract class JavaFXExpressionEntry extends JavaFXModuleEntry with ExpressionEntry

  case class JavaFXModule(val name : String) extends JavaFXScope with Module {

    def entries : Seq[JavaFXModuleEntry] =
      getChildren map (_.getValue)

    def appendEntry(entry : JavaFXModuleEntry) : Unit = 
      getChildren add entry

  }

  case class JavaFXImport(override val name : String) extends JavaFXScope with Import {

    def entries : Seq[JavaFXModuleEntry] =
      getChildren map (_.getValue)

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

    getChildren add LiftFillerEntry

  }

  protected def newModule(name : String) : JavaFXModule = JavaFXModule(name)
  protected def newImport(name : String) : JavaFXImport = JavaFXImport(name)
  protected def newParameter(variable : Variable) : JavaFXParameter = JavaFXParameter(variable)
  protected def newLift(filler : Filler) : JavaFXLift = JavaFXLift(filler)

}
