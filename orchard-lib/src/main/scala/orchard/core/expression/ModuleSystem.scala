/**
  * ModuleSystem.scala - A collection of traits and whatnot for modules
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

trait ModuleSystem {

  type EntryType <: ModuleEntry

  type LiftType <: Lift
  type ModuleType <: Module
  type ParameterType <: Parameter
  type InstantiationType <: Instantiation

  trait ModuleEntry { thisEntry : EntryType =>

    def liftEntry : EntryType

    def name : String
    def parent : Option[Module]

    def parameters : Seq[ParameterType] =
      parent match {
        case None => Seq.empty[ParameterType]
        case Some(p) => {
          val parentParams : Seq[ParameterType] = p.parameters
          val myIndex = p.entries.indexOf(thisEntry)
          val siblingParams : Seq[ParameterType] = p.entries.slice(0, myIndex) flatMap {
            case mp : Parameter => Some(mp.liftParameter)
            case _ => None
          }

          parentParams ++ siblingParams
        }
      }

    def environment : Seq[EntryType] =
      parent match {
        case None => Seq.empty[EntryType]
        case Some(p) => {
          val parentEnv = p.environment
          val myIndex = p.entries.indexOf(thisEntry)
          val siblingEnv = 
            p.entries.slice(0, myIndex) flatMap {
              case mod : Module => None
              case entry @ _ => Some(entry.liftEntry)
            }

          parentEnv ++ siblingEnv
        }
      }
  }

  trait Module extends ModuleEntry with Workspace { 
    thisModule : ModuleType with EntryType =>

    def liftModule : ModuleType

    def entries : Seq[EntryType]

    def localEnvironment : Seq[EntryType] = 
      thisModule.environment ++ (entries filterNot (_.isInstanceOf[Module]))

    def appendParameter(variable : Variable) 
    def appendLift(filler : Filler)
    def appendInstantiation(shell : Shell, reference : ExternalReference, bindings : Map[Int, Expression])

  }


  trait ExpressionEntry extends ModuleEntry { 
    thisEntry : EntryType =>

    def expression : Expression

  }

  trait Parameter extends ExpressionEntry { 
    thisParameter : ParameterType with EntryType =>

    def liftParameter : ParameterType

    def variable : Variable

  }

  trait Lift extends ExpressionEntry { 
    thisLift : LiftType with EntryType =>

    def liftLift : LiftType

    def filler : Filler

  }

  trait Instantiation extends ExpressionEntry {
    thisInstantiation : InstantiationType with EntryType =>

    def liftInstantiation : InstantiationType

    def reference : ExternalReference
    def bindings : Map[Int, Expression]

  }

}
