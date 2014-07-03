/**
  * ModuleSystem.scala - Collections of Traits for Modules
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

import scala.language.implicitConversions

import orchard.core.ui.Styleable

trait ModuleModule { thisChecker : TypeChecker => 

  //============================================================================================
  // TYPE ABSTRACTIONS
  //

  type ModuleEntryType <: ModuleEntry
  type ModuleType <: ModuleEntryType with Module
  type ImportType <: ModuleEntryType with Import
  type ParameterType <: ModuleEntryType with Parameter
  type LiftType <: ModuleEntryType with Lift

  //============================================================================================
  // MODULE TRAITS
  //

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

    def liftModule : ModuleType = thisModule

    // This should be protected to the type checker class
    def appendEntry(entry : ModuleEntryType) : Unit

  }

  trait Import extends Scope {
    thisImport : ImportType =>

    def liftImport : ImportType = this

    def isOpen : Boolean

  }

  trait Parameter extends ModuleEntry {
    thisParameter : ParameterType =>

    def liftParameter : ParameterType = this

    def ident : Identifier
    def isThin : Boolean
    def shell : Shell

    val variable : Variable = Variable(ident, shell, isThin)

  }

  trait Lift extends ModuleEntry {
    thisLift : LiftType =>

    def ident : Identifier
    def nook : Nook

    val filler : Filler = Filler(ident, nook)

    def liftLift : LiftType = this

  }

  //============================================================================================
  // IMPLICIT CONVERSIONS
  //

  object Module {
    implicit def moduleIsModuleType(m : Module) : ModuleType = m.liftModule
  }

  object Import {
    implicit def importIsImportType(i : Import) : ImportType = i.liftImport
  }

  object Parameter {
    implicit def paramIsParamType(p : Parameter) : ParameterType = p.liftParameter
  }

  object Lift {
    implicit def liftIsLiftType(l : Lift) : LiftType = l.liftLift
  }

  //============================================================================================
  // CONSTRUCTORS
  //

  protected def newModule(name : String) : ModuleType
  protected def newImport(name : String) : ImportType
  protected def newParameter(ident : Identifier, shell : Shell, isThin : Boolean) : ParameterType
  protected def newLift(ident : Identifier, nook : Nook) : LiftType

}
