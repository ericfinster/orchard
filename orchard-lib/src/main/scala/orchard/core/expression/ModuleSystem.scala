/**
  * ModuleSystem.scala - Collections of Traits for Modules
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

import orchard.core.ui.Styleable

trait ModuleSystem { thisSystem : TypeChecker => 

  type ModuleEntryType <: ModuleEntry
  type ModuleType <: ModuleEntryType with Module
  type ImportType <: ModuleEntryType with Import
  type ParameterType <: ModuleEntryType with Parameter
  type LiftType <: ModuleEntryType with Lift

  trait ModuleEntry extends Styleable { 
    thisEntry : ModuleEntryType =>

    def parentScope : Option[Scope]

  }

  trait Scope extends ModuleEntry {
    thisScope : ModuleEntryType =>

    def entries : Seq[ModuleEntryType]

  }

  trait Module extends Scope {
    thisModule : ModuleType =>

    // This should be protected to the type checker class
    def appendEntry(entry : ModuleEntryType) : Unit

  }

  trait Import extends Scope {
    thisImport : ImportType =>

    def isOpen : Boolean

  }

  trait Parameter extends ModuleEntry {
    thisParameter : ParameterType =>

  }

  trait Lift extends ModuleEntry {
    thisLift : LiftType =>

  }

  //============================================================================================
  // CONSTRUCTORS
  //

  protected def newModule(name : String) : ModuleType
  protected def newImport(name : String) : ImportType
  protected def newParameter(variable : Variable) : ParameterType
  protected def newLift(filler : Filler) : LiftType

}
