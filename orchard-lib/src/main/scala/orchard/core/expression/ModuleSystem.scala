/**
  * ModuleSystem.scala - A collection of traits and whatnot for modules
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

trait ModuleSystem {

  type EntryType <: ModuleEntry
  type ContainerType <: EntryContainer
  type ModuleType <: Module
  type DefinitionType <: Definition
  type ParameterType <: ModuleParameter

  trait ModuleEntry { thisEntry : EntryType =>

    def liftEntry : EntryType

    def name : String
    def parent : Option[ContainerType]

    def parameters : Seq[ParameterType] =
      parent match {
        case None => Seq.empty[ParameterType]
        case Some(p) => {
          val parentParams : Seq[ParameterType] = p.parameters
          val myIndex = p.entries.indexOf(thisEntry)
          val siblingParams : Seq[ParameterType] = p.entries.slice(0, myIndex) flatMap {
            case mp : ModuleParameter => Some(mp.liftParameter)
            case _ => None
          }

          parentParams ++ siblingParams
        }
      }

  }

  trait EntryContainer extends ModuleEntry { 
    thisContainer : ContainerType with EntryType =>

    def liftContainer : ContainerType

    def entries : Seq[EntryType]

    def localParameters : Seq[ParameterType] =
      entries flatMap {
        case mp : ModuleParameter => Some(mp.liftParameter)
        case _ => None
      }

  }

  trait ModuleParameter extends ModuleEntry { 
    thisParameter : ParameterType with EntryType =>

    def liftParameter : ParameterType

    def variable : Variable

  }

  trait Module extends EntryContainer with Workspace { 
    thisModule : ModuleType with ContainerType with EntryType =>

    def liftModule : ModuleType

  }

  trait Definition extends EntryContainer with Workspace { 
    thisDefinition : DefinitionType with ContainerType with EntryType =>

    def liftDefinition : DefinitionType

    def filler : Option[Filler]
    def isComplete : Boolean = filler != None

  }

}
