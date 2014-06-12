/**
  * JavaFXDefinitionInstantiator.scala - UI code to instantiate a definition
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.Includes._

import scalafx.geometry._
import scalafx.scene.layout._
import scalafx.scene.control._

import orchard.core.cell._
import orchard.core.expression._

import JavaFXModuleSystem._

class JavaFXDefinitionInstantiator(owner : JavaFXWorkspace, shell : Shell, defn : JavaFXDefinition) extends Instantiator { thisInstantiator =>

  def this(owner : JavaFXWorkspace, defn : JavaFXDefinition) = this(owner, owner.emptyShell, defn)

  //============================================================================================
  // SEMANTICS
  //

  val referenceExpression = Reference(defn, Immediate)

  //============================================================================================
  // UI ELEMENTS
  //

  val parameterView = new ListView[JavaFXModuleEntry] {
    cellFactory = (_ => new ModuleListCell)
    items() ++= (defn.parameters ++ defn.localParameters)
  }

  parameterView.selectionModel().selectedItem onChange {
    val entry = parameterView.selectionModel().selectedItem()

    if (entry != null) {
      displayGoal(entry.asInstanceOf[JavaFXModuleParameter].variable)
    }
  }

  def displayGoal(expr : Expression) : Unit = {
    val gallery = new FrameworkGallery(expr)
    goalPane.content = gallery
    gallery.refreshAll
  }

  def displayExpr(expr : Expression) : Unit = {
    val gallery = new FrameworkGallery(expr)
    exprPane.content = gallery
    gallery.refreshAll
  }

  val parameterPane = new TitledPane {
    text = "Parameters"
    content = parameterView
    collapsible = false
  }

  val parameterAnchor = new AnchorPane {
    content = parameterPane
  }

  AnchorPane.setTopAnchor(parameterPane, 10)
  AnchorPane.setRightAnchor(parameterPane, 10)
  AnchorPane.setBottomAnchor(parameterPane, 10)
  AnchorPane.setLeftAnchor(parameterPane, 10)

  val noGoalLabel = new Label("No goal")

  val goalPane = new StackPane {
    content = noGoalLabel
    styleClass += "orch-pane"
  }

  val noExprLabel = new Label("No expression")

  val exprPane = new StackPane {
    content = noExprLabel
    styleClass += "orch-pane"
  }

  val goalRow = new RowConstraints { percentHeight = 50 }
  val exprRow = new RowConstraints { percentHeight = 50 }

  val gridPane = new GridPane {
    styleClass += "orch-pane"
    rowConstraints ++= List(goalRow, exprRow)
  }

  GridPane.setRowSpan(parameterAnchor, 2)
  GridPane.setHgrow(goalPane, Priority.ALWAYS)
  GridPane.setHgrow(exprPane, Priority.ALWAYS)
  GridPane.setVgrow(goalPane, Priority.ALWAYS)
  GridPane.setVgrow(exprPane, Priority.ALWAYS)

  gridPane.add(parameterAnchor, 0, 0)
  gridPane.add(goalPane, 1, 0)
  gridPane.add(exprPane, 1, 1)

  val tab = new Tab { thisTab =>
    text = defn.defnName
    content = gridPane

    onSelectionChanged = () => {
      if (selected()) {
        owner.activeInstantiator = Some(thisInstantiator)
      }
    }
  }

}
