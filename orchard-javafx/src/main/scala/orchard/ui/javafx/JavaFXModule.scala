/**
  * JavaFXModule.scala - JavaFX Modules
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scala.collection.mutable.Buffer

import orchard.core.expression._

class JavaFXModule(
  val name : String,
  val editor : JavaFXEditor,
  val stabilityLevel : Option[Int],
  val invertibilityLevel : Option[Int],
  val unicityLevel : Option[Int]
) extends JavaFXModuleEntry with JavaFXModuleUI with Module {

  type EntryType = JavaFXModuleEntry

  def entries : Seq[JavaFXModuleEntry] =
    treeItem.children map (_.getValue)

  def worksheets : Buffer[Worksheet] = Buffer.empty

  def activeWorksheet : Option[Worksheet] =
    for {
      gallery <- activeGallery
    } yield gallery.complex

}
