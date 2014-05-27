/**
  * Module.scala - Toplevel definitions for modules
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.editor

trait Module extends {

  def imports : Seq[Module]
  def entries : Seq[SectionEntry]

  def stabilityLevel : Option[Int]
  def invertibilityLevel : Option[Int]
  def unicityLevel : Option[Int]

}
