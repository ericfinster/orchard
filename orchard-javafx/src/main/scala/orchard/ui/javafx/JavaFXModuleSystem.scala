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
import orchard.core.ui.Stylable

object JavaFXModuleSystem extends ModuleSystem {

  type EntryType = JavaFXModuleEntry

  type LiftType = JavaFXLift
  type ModuleType = JavaFXModule
  type ParameterType = JavaFXParameter
  type InstantiationType = JavaFXInstantiation

  //============================================================================================
  // MODULE ENTRIES
  //

  abstract class JavaFXModuleEntry extends ModuleEntry with Stylable {

    def owner = JavaFXModuleSystem

    def liftEntry = this
    def styleString : String = "unknown"

    val treeItem = new TreeItem[JavaFXModuleEntry](this)

    def focusModule : JavaFXModule

    def cloneTreeItem : TreeItem[JavaFXModuleEntry] = {
      val newTreeItem = new TreeItem[JavaFXModuleEntry](this)
      newTreeItem.children = (treeItem.children map (_.value().cloneTreeItem))
      newTreeItem
    }

    def toXML : xml.NodeSeq

  }
  
  //============================================================================================
  // PARAMETERS
  //

  class JavaFXParameter(val variable : Variable, val module : JavaFXModule)
      extends JavaFXModuleEntry with ExpressionEntry
      with Parameter {
  
    def liftParameter : JavaFXParameter = this

    def parent : Option[JavaFXModule] = Some(module)

    def name : String = variable.id
    def expression = variable

    override def styleString : String = if (variable.isThin) "variable-thin" else "variable"

    def focusModule = module

    def toXML : xml.NodeSeq = ???

  }
  
  //============================================================================================
  // LIFTS
  //

  class JavaFXLift(val filler : Filler, val module : JavaFXModule) 
      extends JavaFXModuleEntry with ExpressionEntry with Lift { thisLift =>

    def liftLift = this

    def parent = Some(module)

    def name = filler.Boundary.id
    def expression = filler.Boundary

    def focusModule = module

    override def styleString = 
      if (filler.Boundary.isThin) "bdry-thin" else "bdry"

    def toXML : xml.NodeSeq = ???

    object FillerEntry extends JavaFXModuleEntry with ExpressionEntry {

      def parent = Some(module)  // This is suspicious ...

      def name : String = filler.id
      def expression = filler

      def focusModule = module

      override def styleString = "filler"

      def toXML : xml.NodeSeq = ???

    }

    treeItem.children += FillerEntry.treeItem

  }

  //============================================================================================
  // INSTANTIATIONS
  //

  class JavaFXInstantiation(val shell : Shell, val reference : ExternalReference, val bindings : Map[Int, Expression], val module : JavaFXModule) 
      extends JavaFXModuleEntry with ExpressionEntry with Instantiation {

    def liftInstantiation = this
    def liftLift = this

    def name = expression.id
    def parent = Some(module)

    def focusModule = module

    override def styleString = "app"

    val expression : Expression = {
      val bdryAddr = reference.expression.asInstanceOf[Filler].bdryAddress
      FillerEntry.expression.ncell.seek(bdryAddr).get.value
    }

    def toXML : xml.NodeSeq = ???

    object FillerEntry extends JavaFXModuleEntry with ExpressionEntry {

      def name : String = expression.id
      def parent = Some(module)  // This is suspicious ...

      def focusModule = module

      override def styleString = "app"

      val expression = {
        val subst = Substitution(shell, reference, bindings)
        subst.entry = Some(this)
        subst
      }

      def toXML : xml.NodeSeq = ???

    }

    treeItem.children += FillerEntry.treeItem

  }

  //============================================================================================
  // MODULES
  //

  class JavaFXModule(
      val name : String,
      val parent : Option[JavaFXModule],
      val stabilityLevel : Option[Int],
      val invertibilityLevel : Option[Int],
      val unicityLevel : Option[Int]
  ) extends JavaFXModuleEntry
      with JavaFXWorkspace
      with Module { thisModule =>

    def liftModule : JavaFXModule = this

    // Not sure I really want this ...
    def entries : Seq[JavaFXModuleEntry] =
      treeItem.children map (_.getValue)

    def appendLift(filler : Filler) : Unit = {
      val liftEntry = new JavaFXLift(filler, thisModule)
      filler.entry = Some(liftEntry)
      treeItem.children += liftEntry.treeItem
      editor.displayEnvironment
    }

    def appendParameter(variable : Variable) : Unit = {
      val paramEntry = new JavaFXParameter(variable, thisModule)
      variable.entry = Some(paramEntry)
      treeItem.children += paramEntry.treeItem
      editor.displayEnvironment
    }

    def appendInstantiation(shell : Shell, reference : ExternalReference, bindings : Map[Int, Expression]) {
      treeItem.children += new JavaFXInstantiation(shell, reference, bindings, thisModule).treeItem
      editor.displayEnvironment
    }

    def focusModule = thisModule

    def toXML : xml.NodeSeq =
        <module name={name}
          slevel={(stabilityLevel getOrElse -1).toString}
          ilevel={(invertibilityLevel getOrElse -1).toString}
          ulevel={(unicityLevel getOrElse -1).toString}>{
            entries flatMap (_.toXML)
          }</module>
  }

  //============================================================================================
  // DEFINITIONS
  //

  // class JavaFXDefinition(val defnName : String, val module : JavaFXModule)
  //     extends JavaFXEntryContainer
  //     with JavaFXWorkspace
  //     with Definition { thisDefinition =>

  //   def liftDefinition : JavaFXDefinition = this

  //   // Instead of this, you should style it with css somehow ..
  //   def name : String = "Definition: " ++ defnName
  //   def parent : Option[JavaFXEntryContainer] = Some(module)

  //   private var myFiller : Option[Filler] = None
    
  //   def filler : Option[Filler] = myFiller
  //   def filler_=(fillerOpt : Option[Filler]) : Unit = {
  //     myFiller = fillerOpt

  //     fillerOpt match {
  //       case None => treeItem.children -= BoundaryEntry.treeItem
  //       case Some(_) => treeItem.children += BoundaryEntry.treeItem
  //     }
  //   }

  //   val stabilityLevel : Option[Int] = module.stabilityLevel
  //   val invertibilityLevel : Option[Int] = module.invertibilityLevel
  //   val unicityLevel : Option[Int] = module.unicityLevel

  //   def focusModule : JavaFXModule = module
  //   def focusWorkspace : JavaFXWorkspace = thisDefinition

  //   override def styleString = if (isComplete) "defn-complete" else "defn-incomplete"

  //   object BoundaryEntry extends JavaFXEntryContainer { thisBdryEntry =>

  //     def name : String =
  //       (filler map (_.Boundary.id)) getOrElse "Unknown"

  //     def parent = Some(thisDefinition)

  //     def focusModule : JavaFXModule = module
  //     def focusWorkspace : JavaFXWorkspace = thisDefinition

  //     override def styleString = 
  //       (filler map (f => if (f.Boundary.isThin) "bdry-thin" else "bdry")) getOrElse "unknown"

  //     object FillerEntry extends JavaFXModuleEntry {

  //       def name : String =
  //         (filler map (_.id)) getOrElse "Unknown"

  //       def parent = Some(thisBdryEntry)

  //       def focusModule : JavaFXModule = module
  //       def focusWorkspace : JavaFXWorkspace = thisDefinition

  //       override def styleString = "filler"

  //     }

  //     treeItem.children += FillerEntry.treeItem
  //   }

  //   override def assumeAtSelection(thinHint : Boolean) : Unit = {
  //     if (isComplete) {
  //       editor.consoleError("Cannot assume new variable in a complete definition.")
  //     } else {
  //       super.assumeAtSelection(thinHint)
  //     }
  //   }

  //   override def fillAtSelection : Unit = {
  //     if (isComplete) {
  //       editor.consoleError("Cannot create filler in a complete definition.")
  //     } else {
  //       super.fillAtSelection
  //     }
  //   }

  // }

}
