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

    def qualifiedName : String = 
      parentScope match {
        case None => name
        case Some(scope) => scope.qualifiedName ++ "/" ++ name
      }

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

    def styleString = "unknown"

  }

  trait Import extends Scope {
    thisImport : ImportType =>

    def liftImport : ImportType = this

    def isOpen : Boolean

    def name = "unknown"
    def styleString = "unknown"

  }

  trait ExpressionEntry extends ModuleEntry {
    thisEntry : ModuleEntryType =>

    type ExpressionType <: Expression

    def expression : ExpressionType

    def name : String = expression.name
    def isThin : Boolean = expression.isThin
    def styleString : String = expression.styleString

  }

  trait Parameter extends ExpressionEntry {
    thisParameter : ParameterType =>

    type ExpressionType = Variable

    def liftParameter : ParameterType = this

  }

  trait Lift extends ExpressionEntry {
    thisLift : LiftType =>

    type ExpressionType = Filler#BoundaryExpr

    def liftLift : LiftType = this
    def fillerEntry : FillerEntry

    trait FillerEntry extends ExpressionEntry {
      thisFillerEntry : ModuleEntryType =>

      type ExpressionType = Filler

      def liftFillerEntry : ModuleEntryType = this

    }

    object FillerEntry {
      implicit def fillerEntryIsEntry(f : FillerEntry) : ModuleEntryType = f.liftFillerEntry
    }

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
  protected def newParameter(varaible : Variable) : ParameterType
  protected def newLift(filler : Filler) : LiftType

}
