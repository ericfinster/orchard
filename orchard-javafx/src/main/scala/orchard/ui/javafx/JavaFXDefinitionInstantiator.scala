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

class JavaFXLiftInstantiator(val wksp : JavaFXWorkspace, val shell : Shell, val lift : JavaFXLift) extends Instantiator { thisInstantiator =>

  def this(wksp : JavaFXWorkspace, lift : JavaFXLift) = this(wksp, wksp.emptyShell, lift)

  //============================================================================================
  // SEMANTICS
  //

  var myActiveGoal : Option[Goal] = None

  def activeGoal : Option[Goal] = myActiveGoal
  def activeGoal_=(goalOpt : Option[Goal]) = {
    myActiveGoal = goalOpt

    for { goal <- goalOpt } {
      val gallery = new FrameworkGallery(Substitution(goal, bindings))
      goalPane.content = gallery
      gallery.refreshAll
    }
  }

  var myActiveExpression : Option[Expression] = None

  def activeExpression : Option[Expression] = myActiveExpression
  def activeExpression_=(exprOpt : Option[Expression]) = {
    myActiveExpression = exprOpt

    for { expr <- exprOpt } {
      val gallery = new FrameworkGallery(expr)
      exprPane.content = gallery
      gallery.refreshAll
    }
  }

  def refreshPreview : Unit = {
    // Refresh the list view
    parameterView.items().clear
    parameterView.items() ++= goals

    val gallery = new FrameworkGallery(previewExpression)
    outputPane.content = gallery
    gallery.refreshAll
  }

  //============================================================================================
  // UI ELEMENTS
  //

  val parameterView = new ListView[Expression] {
    cellFactory = (_ => new ExpressionListCell)
  }

  parameterView.selectionModel().selectedItem onChange {
    val goal = parameterView.selectionModel().selectedItem()

    if (goal != null) {
      activeGoal = Some(goal.asInstanceOf[Goal])
    }
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
    text = lift.name
    content = gridPane

    onSelectionChanged = () => {
      if (selected()) {
        wksp.activeInstantiator = Some(thisInstantiator)
      }
    }
  }

  //============================================================================================
  // INITIALIZATION
  //

  refreshPreview

}
