/**
  * JavaFXSection.scala - JavaFX Section Routines
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scala.collection.mutable.Buffer

import orchard.core.expression._

class JavaFXSection extends JavaFXSectionEntry with Section {

  type SectionEntryType = JavaFXSectionEntry

  def module : JavaFXModule = ???

  def entries : Buffer[JavaFXSectionEntry] = ???

}
