/**
  * Module.scala - Toplevel definitions for modules
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

import scala.collection.mutable.Buffer

trait Module extends EntryContainer { thisModule : Scope =>

  type ModuleType <: Module

  def imports : Buffer[ModuleType]

  def stabilityLevel : Option[Int]
  def invertibilityLevel : Option[Int]
  def unicityLevel : Option[Int]

}
