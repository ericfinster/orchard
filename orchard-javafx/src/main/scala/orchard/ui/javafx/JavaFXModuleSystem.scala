/**
  * JavaFXModuleSystem.scala - Module system UI Elements
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scala.collection.mutable.Buffer

import scalafx.Includes._
import scalafx.scene.control._

import orchard.core.expression._

object JavaFXModuleSystem extends ModuleSystem {

  type EntryType = JavaFXModuleEntry
  type ContainerType = JavaFXEntryContainer
  type ModuleType = JavaFXModule
  type DefinitionType = JavaFXDefinition
  type ParameterType = JavaFXModuleParameter

  //============================================================================================
  // MODULE ENTRIES
  //

  abstract class JavaFXModuleEntry extends ModuleEntry {

    def liftEntry = this
    def styleString : String = "unknown"

    val treeItem = new TreeItem[JavaFXModuleEntry](this)

  }
  
  //============================================================================================
  // ENTRY CONTAINERS
  //

  abstract class JavaFXEntryContainer 
      extends JavaFXModuleEntry
      with EntryContainer {

    def liftContainer : JavaFXEntryContainer = this

    // Not sure I really want this ...
    def entries : Seq[JavaFXModuleEntry] =
      treeItem.children map (_.getValue)
    
  }
  
  //============================================================================================
  // MODULES
  //

  class JavaFXModule(
      val name : String,
      val parent : Option[JavaFXEntryContainer],
      val editor : JavaFXEditor,
      val stabilityLevel : Option[Int],
      val invertibilityLevel : Option[Int],
      val unicityLevel : Option[Int]
  ) extends JavaFXEntryContainer 
      with JavaFXModuleUI
      with Module { thisModule =>

    def liftModule : JavaFXModule = this

    def worksheets : Buffer[Worksheet] = Buffer.empty

    def activeWorksheet : Option[Worksheet] =
      for {
        gallery <- activeGallery
      } yield gallery.complex

    def appendVariable(variable : Variable) : Unit = {
      val newVar = new JavaFXModuleParameter(Some(thisModule), variable)
      treeItem.children += newVar.treeItem
      editor.moduleView.selectionModel().select(newVar.treeItem)
    }

    def appendDefinition(filler : Filler) : Unit = {
      val defn = new JavaFXDefinition(Some(thisModule), filler)
      treeItem.children += defn.treeItem
      editor.moduleView.selectionModel().select(defn.treeItem)
    }

    def appendSubmodule(subName : String) : Unit = {
      val subMod = new JavaFXModule(subName, Some(thisModule), editor, stabilityLevel, invertibilityLevel, unicityLevel)
      treeItem.children += subMod.treeItem
      editor.moduleView.selectionModel().select(subMod.treeItem)
      subMod.newSheet
    }

    //   def toXML : xml.NodeSeq =
    //     <module name={name}
    //   slevel={(stabilityLevel getOrElse -1).toString}
    //   ilevel={(invertibilityLevel getOrElse -1).toString}
    //   ulevel={(unicityLevel getOrElse -1).toString}>{
    //     entries flatMap (_.toXML)
    //   }</module>

  }

  //============================================================================================
  // MODULE PARAMETERS
  //

  class JavaFXModuleParameter(val parent : Option[JavaFXEntryContainer], val variable : Variable) 
      extends JavaFXModuleEntry 
      with ModuleParameter {
  
    def liftParameter : JavaFXModuleParameter = this

    def name : String = variable.id
    override def styleString : String = if (variable.isThin) "var-thin" else "var"

  }

  //============================================================================================
  // DEFINITIONS
  //

  class JavaFXDefinition(val parent : Option[JavaFXEntryContainer], val filler : Filler)
      extends JavaFXEntryContainer
      with Definition { thisDefinition =>

    def liftDefinition : JavaFXDefinition = this

    def name : String = filler.Boundary.id
    def localParameters : Seq[JavaFXModuleParameter] = Seq.empty

  }

}
