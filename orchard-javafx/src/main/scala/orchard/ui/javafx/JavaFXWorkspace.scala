/**
  * JavaFXWorkspace.scala - JavaFX implementation of a workspace
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.Includes._

import scalafx.geometry._

import scalafx.scene.layout._
import scalafx.scene.control._

import orchard.core.ui._
import orchard.core.cell._
import orchard.core.editor._
import orchard.core.complex._
import orchard.core.expression._

import javafx.scene.{control => jfxsc}

class JavaFXWorkspace(
  val editor : JavaFXEditor,
  val name : String,
  val stabilityLevel : Option[Int],
  val invertibilityLevel : Option[Int],
  val unicityLevel : Option[Int]
) extends Workspace with JavaFXWorksheetEnv {

  type SubstitutionType = JavaFXSubstitution
  type EnvironmentNodeType = jfxsc.TreeItem[EnvironmentElement]

  var activeExpression : Option[NCell[Expression]] = None
  var activeGallery : Option[WorksheetGallery] = None 

  def activeSheet : Option[Worksheet] =
    for {
      gallery <- activeGallery
    } yield gallery.complex

  def activeSubstitution : Option[JavaFXSubstitution] = {
    val pane = 
      substitutionAccordion.expandedPane().
        asInstanceOf[JavaFXSubstitution#SubstitutionPane]

    if (pane != null) {
      Some(pane.substitution)
    } else None
  }

  var sheetCount : Int = 1

  val sheetTabPane = new TabPane {
    side = Side.TOP
  }

  def newSheet = newSheet(CardinalComplex(Object(None)))

  def newSheet(seed : NCell[Polarity[Option[Expression]]]) : Unit = {
    val gallery = new WorksheetGallery(seed)

    val tab = new Tab {
      text = "Sheet " ++ sheetCount.toString
      content = gallery

      onClosed = () => {
        sheets -= gallery.complex
      }

      onSelectionChanged = () => {
        if (selected())
          activeGallery = Some(gallery)
      }
    }

    sheetTabPane += tab
    sheetTabPane.getSelectionModel.select(tab)
    sheets += gallery.complex
    sheetCount += 1
    gallery.refreshAll

  }

  def newSubstitutionInShell(defn : Definition, shell : NCell[Option[Expression]]) = {
    val newSubst = new JavaFXSubstitution(this, defn, shell)
    substitutionAccordion.panes += newSubst.substitutionPane
    substitutionAccordion.expandedPane = newSubst.substitutionPane
  }

  def closeSubstitution(subst : JavaFXSubstitution) : Unit = {
    substitutionAccordion.panes -= subst.substitutionPane
  }

  def importActiveSubstitution : Unit =
    for {
      subst <- activeSubstitution
    } { 
      if (addToEnvironment(envOps.shallowClone(subst.envRoot))) {
        closeSubstitution(subst)    
      } 
    }

  val substitutionAccordion = new Accordion

  val envOps = JavaFXEnvironment
  val envRoot = envOps.createNode(GroupElement(name))

  val environmentView = 
    new TreeView[EnvironmentElement] {
      root = envRoot
      showRoot = false
      cellFactory = (_ => new JavaFXEnvironment.EnvironmentTreeCell)
    }

  environmentView.getSelectionModel.selectedItem onChange {
    val item = environmentView.getSelectionModel.selectedItem()

    if (item != null) {
      item.value() match {
        case ExpressionElement(ncell) => {
          activeExpression = Some(ncell)

          // Now make a gallery and show it in the preview pane
          val gallery = new FrameworkGallery(ncell map (Some(_)))
          editor.setPreviewGallery(gallery)
        }
        case _ => activeExpression = None
      }
    } else {
      activeExpression = None
    }
  }

  // val environment
  // val previewContent = 
  //   new VBox {
  //     content = List()
  //   }
}
