/**
  * JavaFXSubstitution.scala - JavaFX visualized substitution implementation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.Includes._

import scalafx.scene.layout._
import scalafx.scene.control._

import orchard.core.ui._
import orchard.core.cell._
import orchard.core.editor._
import orchard.core.complex._
import orchard.core.expression._

import javafx.scene.{control => jfxsc}

class JavaFXSubstitution(val wksp : JavaFXWorkspace, defn : Definition, shell : NCell[Option[Expression]]) 
    extends Substitution(wksp, defn, shell) { thisSubstitution =>

  type EnvironmentNodeType = jfxsc.TreeItem[EnvironmentElement]

  trait SubstitutionElement {
    def substitution = thisSubstitution
  }

  val envOps = JavaFXEnvironment

  val envRoot = {
    val newRoot = JavaFXEnvironment.cloneFrom(defn.envRoot, defn.envOps)

    envOps.mapExprs(newRoot, (expr => {
      val shellFramework = new SimpleFramework(shell)
      shellFramework.stablyAppend(new SimpleFramework(expr map (Some(_))))
      shellFramework.toCell map (_.get)
    }))

    newRoot
  }

  setupDependencies

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
          // Now make a gallery and show it in the preview pane
          val gallery = new FrameworkGallery(ncell map (Some(_)))
          wksp.editor.setGoalPreviewGallery(gallery)
        }
        case _ => ()
      }
    } 
  }

  class SubstitutionPane extends jfxsc.TitledPane with SubstitutionElement

  val substitutionPane = new SubstitutionPane 
  substitutionPane.setText(defn.name)
  substitutionPane.setContent(environmentView)

  def activeExpression : Option[NCell[Expression]] = {
    val item = environmentView.getSelectionModel.selectedItem()

    if (item != null) {
      item.value() match {
        case GroupElement(_) => None
        case ExpressionElement(ncell) => Some(ncell)
      }
    } else None
  }

}
