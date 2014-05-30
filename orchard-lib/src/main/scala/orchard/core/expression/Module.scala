/**
  * Module.scala - Modules
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

trait Module extends ModuleEntry {

  type EntryType <: ModuleEntry

  def entries : Seq[EntryType]

}
