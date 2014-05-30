/**
  * ModuleEntry.scala - Trait for entities which can live in a module
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

trait ModuleEntry { thisEntry : ModuleEnvironment#ModuleEntry =>

//   type EntryType <: ModuleEntry
//   type ModuleType <: Module
//   type VariableType <: ModuleVariable

//   def name : String
//   def parent : Option[ModuleType]

//   def parameters : Seq[ModuleVariable] = 
//     parent match {
//       case None => Seq.empty[ModuleVariable]
//       case Some(p) => {
//         val parentParams = p.parameters
//         val myIndex = p.entries.indexOf(thisEntry)
//         val siblingParams = p.entries.slice(0, myIndex) flatMap {
//           case mv : ModuleVariable => Some(mv)
//           case _ => None
//         }

//         parentParams ++ siblingParams
//       }
//     }


}
