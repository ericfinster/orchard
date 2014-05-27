/**
  * Section.scala - Definitions of sections
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.editor

trait Section extends SectionEntry {

  def entries : Seq[SectionEntry]

}
