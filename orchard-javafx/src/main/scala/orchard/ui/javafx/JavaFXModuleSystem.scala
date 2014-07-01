/**
  * JavaFXModuleSystem.scala - UI Elements for the module system
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.Includes._
import scalafx.scene.control._

import orchard.core.ui.Stylable
import orchard.core.expression._

object JavaFXModuleSystem extends ModuleSystem {

  type EntryType = JavaFXModuleEntry
  type ModuleType = JavaFXModule
  type ParameterType = JavaFXParameter
  type LiftType = JavaFXLift
  type InstantiationType = JavaFXInstantiation

  abstract class JavaFXModuleEntry extends TreeItem[JavaFXModuleEntry] with ModuleEntry with Stylable {

    def parentModule : Option[JavaFXModule] = {
      val parentItem = parent()

      if (parentItem == null) None else 
        parentItem.value() match {
          case mod : JavaFXModule => Some(mod)
          case _ => None
        }
    }

    // Initialization
    value = this

  }

  class JavaFXModule(val name : String) extends JavaFXModuleEntry with Module {

    def entries : Seq[JavaFXModuleEntry] =
      children map (_.value())

    def appendSubmodule(subMod : JavaFXModule) : Unit =
      children += subMod

    def styleString : String = "unknown"

    override def toString = name
  }

  class JavaFXParameter extends JavaFXModuleEntry with Parameter {

    def name : String = "Untitled"
    def styleString : String = "unknown"

  }

  class JavaFXLift extends JavaFXModuleEntry with Lift {

    def name : String = "Untitled"
    def styleString : String = "unknown"

  }

  class JavaFXInstantiation extends JavaFXModuleEntry with Instantiation {

    def name : String = "Untitled"
    def styleString : String = "unknown"

  }

}
