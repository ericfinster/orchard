/**
  * ModuleEnvironment.scala - A collection of traits and whatnot for modules
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

trait ModuleEnvironment {

  type EditorType <: Editor
  type EntryType <: ModuleEntry
  type ModuleType <: Module
  type VariableType <: ModuleVariable

  trait ModuleEntry { thisEntry =>

    def name : String
    def parent : Option[ModuleType]

    def liftEntry : EntryType

    def parameters : Seq[VariableType] =
      parent match {
        case None => Seq.empty[VariableType]
        case Some(p) => {
          val parentParams : Seq[VariableType] = p.parameters
          val myIndex = p.entries.indexOf(thisEntry)
          val siblingParams : Seq[VariableType] = p.entries.slice(0, myIndex) flatMap {
            case mv : ModuleVariable => Some(mv.liftVariable)
            case _ => None
          }

          parentParams ++ siblingParams
        }
      }

  }

  trait Module extends ModuleEntry { thisModule : ModuleType =>

    def liftModule : ModuleType

    def editor : EditorType
    def entries : Seq[EntryType]

    def localParameters : Seq[VariableType] =
      entries flatMap {
        case mv : ModuleVariable => Some(mv.liftVariable)
        case _ => None
      }

    def appendParameter(varExpr : Variable) : Unit
    def appendDefinition(fillerExpr : Filler) : Unit
    //def appendSubmodule(??????)

    def stabilityLevel : Option[Int]
    def invertibilityLevel : Option[Int]
    def unicityLevel : Option[Int]

  }

  trait ModuleVariable extends ModuleEntry { thisVariable : VariableType =>

    def liftVariable : VariableType

    def name : String = varExpr.id
    def varExpr : Variable

  }

}
