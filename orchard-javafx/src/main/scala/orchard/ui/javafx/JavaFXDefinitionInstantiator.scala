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

class JavaFXDefinitionInstantiator(val owner : JavaFXWorkspace, val shell : Shell, val defn : JavaFXDefinition) extends Instantiator { thisInstantiator =>

  def this(owner : JavaFXWorkspace, defn : JavaFXDefinition) = this(owner, owner.emptyShell, defn)

  // def goals : Seq[JavaFXModuleParameter] = 
  //   (defn.parameters ++ defn.localParameters) map ((mp : JavaFXModuleParameter) =>
  //     new JavaFXModuleParameter(defn, new Goal(mp.variable))
  //   )

  //============================================================================================
  // UI ELEMENTS
  //

  val parameterView = new ListView[Expression] {
    cellFactory = (_ => new ExpressionListCell)
    items() ++= goals
  }

  parameterView.selectionModel().selectedItem onChange {
    val entry = parameterView.selectionModel().selectedItem()

    if (entry != null) {
      displayGoal(entry)
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

  def displayOutput(expr : Expression) : Unit = {
    val gallery = new FrameworkGallery(expr)
    outputPane.content = gallery
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

  val outputPane = new StackPane {
    styleClass += "orch-pane"
  }

  val goalRow = new RowConstraints { percentHeight = 33 }
  val exprRow = new RowConstraints { percentHeight = 33 }
  val outputRow = new RowConstraints { percentHeight = 34 }
  
  val gridPane = new GridPane {
    styleClass += "orch-pane"
    rowConstraints ++= List(goalRow, exprRow)
  }

  GridPane.setRowSpan(parameterAnchor, 3)
  GridPane.setHgrow(goalPane, Priority.ALWAYS)
  GridPane.setHgrow(exprPane, Priority.ALWAYS)
  GridPane.setHgrow(outputPane, Priority.ALWAYS)
  GridPane.setVgrow(goalPane, Priority.ALWAYS)
  GridPane.setVgrow(exprPane, Priority.ALWAYS)
  GridPane.setVgrow(outputPane, Priority.ALWAYS)

  gridPane.add(parameterAnchor, 0, 0)
  gridPane.add(goalPane, 1, 0)
  gridPane.add(exprPane, 1, 1)
  gridPane.add(outputPane, 1, 2)

  val tab = new Tab { thisTab =>
    text = defn.defnName
    content = gridPane

    onSelectionChanged = () => {
      if (selected()) {
        owner.activeInstantiator = Some(thisInstantiator)
      }
    }
  }

  //============================================================================================
  // INITIALIZATION
  //

  displayOutput(referenceExpression)

}
