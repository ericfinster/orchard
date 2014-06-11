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

  }

  trait Module extends EntryContainer with Workspace { 
    thisModule : ModuleType with ContainerType with EntryType =>

    def liftModule : ModuleType

    def localParameters : Seq[ParameterType] =
      entries flatMap {
        case mp : ModuleParameter => Some(mp.liftParameter)
        case _ => None
      }

    override def variables : Seq[Variable] = 
      (parameters ++ localParameters) map (_.variable)

  }

  trait Definition extends EntryContainer { 
    thisDefinition : DefinitionType with ContainerType with EntryType =>

    def liftDefinition : DefinitionType

    def localParameters : Seq[ParameterType]
    // def expression : Option[ExpressionType]

    override def entries = 
      (localParameters map (_.liftEntry)) // ++ 
        // (expression map (_.liftEntry))

  }

  trait ModuleParameter extends ModuleEntry { 
    thisParameter : ParameterType with EntryType =>

    def liftParameter : ParameterType

    def variable : Variable

  }

}
