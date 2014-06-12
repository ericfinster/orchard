/**
  * JavaFXModuleSystem.scala - Module system UI Elements
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

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

    def focusModule : JavaFXModule
    def focusWorkspace : JavaFXWorkspace

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
  // MODULE PARAMETERS
  //

  class JavaFXModuleParameter(val owner : JavaFXEntryContainer, val variable : Variable) 
      extends JavaFXModuleEntry 
      with ModuleParameter {
  
    def liftParameter : JavaFXModuleParameter = this

    def name : String = variable.id
    def parent : Option[JavaFXEntryContainer] = Some(owner)

    override def styleString : String = if (variable.isThin) "var-thin" else "var"

    def focusModule : JavaFXModule = 
      owner match {
        case mod : JavaFXModule => mod
        case defn : JavaFXDefinition => defn.owner
      }

    def focusWorkspace : JavaFXWorkspace =
      owner match {
        case mod : JavaFXModule => mod
        case defn : JavaFXDefinition => defn
      }
  }
  
  //============================================================================================
  // MODULES
  //

  class JavaFXModule(
      val name : String,
      val parent : Option[JavaFXEntryContainer],
      val stabilityLevel : Option[Int],
      val invertibilityLevel : Option[Int],
      val unicityLevel : Option[Int]
  ) extends JavaFXEntryContainer 
      with JavaFXWorkspace
      with Module { thisModule =>

    def liftModule : JavaFXModule = this

    def focusModule : JavaFXModule = thisModule
    def focusWorkspace : JavaFXWorkspace = thisModule

    //   def toXML : xml.NodeSeq =
    //     <module name={name}
    //   slevel={(stabilityLevel getOrElse -1).toString}
    //   ilevel={(invertibilityLevel getOrElse -1).toString}
    //   ulevel={(unicityLevel getOrElse -1).toString}>{
    //     entries flatMap (_.toXML)
    //   }</module>

  }

  //============================================================================================
  // DEFINITIONS
  //

  class JavaFXDefinition(val defnName : String, val owner : JavaFXModule)
      extends JavaFXEntryContainer
      with JavaFXWorkspace
      with Definition { thisDefinition =>

    def liftDefinition : JavaFXDefinition = this

    // Instead of this, you should style it with css somehow ..
    def name : String = "Definition: " ++ defnName
    def parent : Option[JavaFXEntryContainer] = Some(owner)

    private var myFiller : Option[Filler] = None
    
    def filler : Option[Filler] = myFiller
    def filler_=(fillerOpt : Option[Filler]) : Unit = {
      myFiller = fillerOpt

      fillerOpt match {
        case None => treeItem.children -= BoundaryEntry.treeItem
        case Some(_) => treeItem.children += BoundaryEntry.treeItem
      }
    }

    val stabilityLevel : Option[Int] = owner.stabilityLevel
    val invertibilityLevel : Option[Int] = owner.invertibilityLevel
    val unicityLevel : Option[Int] = owner.unicityLevel

    def focusModule : JavaFXModule = owner
    def focusWorkspace : JavaFXWorkspace = thisDefinition

    def isComplete : Boolean = filler != None

    override def styleString = if (isComplete) "defn-complete" else "defn-incomplete"

    object BoundaryEntry extends JavaFXEntryContainer { thisBdryEntry =>

      def name : String =
        (filler map (_.Boundary.id)) getOrElse "Unknown"

      def parent = Some(thisDefinition)

      def focusModule : JavaFXModule = owner
      def focusWorkspace : JavaFXWorkspace = thisDefinition

      override def styleString = 
        (filler map (f => if (f.Boundary.isThin) "bdry-thin" else "bdry")) getOrElse "unknown"

      object FillerEntry extends JavaFXModuleEntry {

        def name : String =
          (filler map (_.id)) getOrElse "Unknown"

        def parent = Some(thisBdryEntry)

        def focusModule : JavaFXModule = owner
        def focusWorkspace : JavaFXWorkspace = thisDefinition

        override def styleString = "filler"

      }

      treeItem.children += FillerEntry.treeItem
    }

    override def assumeAtSelection(thinHint : Boolean) : Unit = {
      if (isComplete) {
        editor.consoleError("Cannot assume new variable in a complete definition.")
      } else {
        super.assumeAtSelection(thinHint)
      }
    }

    override def fillAtSelection : Unit = {
      if (isComplete) {
        editor.consoleError("Cannot create filler in a complete definition.")
      } else {
        super.fillAtSelection
      }
    }

  }

}
