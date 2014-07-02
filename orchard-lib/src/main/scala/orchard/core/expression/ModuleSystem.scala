/**
  * ModuleSystem.scala - Collections of Traits for Modules
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

trait ModuleSystem {

  type EntryType <: ModuleEntry
  type ModuleType <: Module
  type ParameterType <: Parameter
  type LiftType <: Lift
  type InstantiationType <: Instantiation

  trait ModuleEntry { thisEntry : EntryType =>

    def parentModule : Option[ModuleType]

  }

  trait Module extends ModuleEntry {
    thisModule : EntryType with ModuleType =>

    def entries : Seq[EntryType]

    def appendSubmodule(subMod : ModuleType) : Unit

    def environment : Environment = 
      parentModule match {
        case None => ???
        case Some(prnt) => {
          // select the entries of the parent up the this guy.
        }

  }

  trait Parameter extends ModuleEntry {
    thisParameter : EntryType with ParameterType =>

  }

  trait Lift extends ModuleEntry {
    thisLift : EntryType with LiftType =>

  }

  trait Instantiation extends ModuleEntry {
    thisInstantiation : EntryType with InstantiationType =>

  }

}
