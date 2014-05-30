/**
  * JavaFXModule.scala - JavaFX Modules
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scala.collection.mutable.Buffer

import scalafx.Includes._

import orchard.core.expression._

class JavaFXModule(
  val name : String,
  val parent : Option[JavaFXModule],
  val editor : JavaFXEditor,
  val stabilityLevel : Option[Int],
  val invertibilityLevel : Option[Int],
  val unicityLevel : Option[Int]
) extends JavaFXModuleEntry with JavaFXModuleUI with JavaFXModuleEnvironment.Module with Module { thisModule =>

  def liftModule = this

  def entries : Seq[JavaFXModuleEntry] =
    treeItem.children map (_.getValue)

  def worksheets : Buffer[Worksheet] = Buffer.empty

  def activeWorksheet : Option[Worksheet] =
    for {
      gallery <- activeGallery
    } yield gallery.complex

  def appendParameter(ve : Variable) : Unit = {
    val newVar = new JavaFXModuleVariable(Some(thisModule), ve)
    treeItem.children += newVar.treeItem
    //moduleView.selectionModel().select(newVar.treeItem)
  }

  def appendDefinition(fillerExpr : Filler) : Unit = ()

  def appendSubmodule(subName : String) : Unit = {
    val subMod = new JavaFXModule(subName, Some(thisModule), editor, stabilityLevel, invertibilityLevel, unicityLevel)
    treeItem.children += subMod.treeItem
    editor.moduleView.selectionModel().select(subMod.treeItem)
    subMod.newSheet
  }


}
