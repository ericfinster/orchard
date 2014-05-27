/**
  * EntryContainer.scala - A Trait for Entry Containers
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

import scala.collection.mutable.Buffer

trait EntryContainer {

  type SectionEntryType <: SectionEntry

  def entries : Buffer[SectionEntryType]

}
