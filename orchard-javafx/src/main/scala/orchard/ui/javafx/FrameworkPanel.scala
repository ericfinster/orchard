/**
  * FrameworkPanel.scala - A panel for displaying expression frameworks
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import orchard.core._

import scalafx.scene.text.Text
import scalafx.scene.layout.Region

import scalafx.scene.paint.Color

import javafx.scene.Node

class FrameworkPanel(val complex : SimpleFramework, baseIndex : Int) extends JavaFXPanel[Option[Expression]] { thisPanel =>

  type CellType = FrameworkCell
  type EdgeType = FrameworkEdge

  type ComplexType = SimpleFramework

  def newCell(owner : complex.CellType) = { 
    val frameworkCell = new FrameworkCell(owner)
    owner.registerPanelCell(thisPanel)(frameworkCell)
    frameworkCell
  }

  def newEdge(owner : complex.CellType) = {
    val frameworkEdge = new FrameworkEdge(owner)
    owner.registerPanelEdge(thisPanel)(frameworkEdge)
    frameworkEdge
  }

  class FrameworkCell(owner : complex.CellType) extends JavaFXCell(owner) {

    def renderLabel : Node = {
      val labelNode = 
        item match {
          case None => new Region { prefWidth = 10 ; prefHeight = 10 }
          case Some(expr) => new Text(expr.id)
        }

      pane.getChildren.setAll(labelNode)
      labelNode
    }

    def assignStyle = 
      item match {
        case None => getStyleClass.add("expr-cell-empty")
        case Some(Variable(_, false)) => getStyleClass.add("expr-cell-var")
        case Some(Variable(_, true)) => getStyleClass.add("expr-cell-var-thin")
        case Some(Filler(_, _)) => getStyleClass.add("expr-cell-filler")
        case Some(FillerTarget(_, _, false)) => getStyleClass.add("expr-cell-filler-tgt")
        case Some(FillerTarget(_, _, true)) => getStyleClass.add("expr-cell-filler-tgt-thin")
      }

    assignStyle

    //============================================================================================
    // HOVER AND SELECTION
    //

    override def doHover = {
      item match {
        case None => getStyleClass.add("expr-cell-empty-hovered")
        case Some(Variable(_, false)) => getStyleClass.add("expr-cell-var-hovered")
        case Some(Variable(_, true)) => getStyleClass.add("expr-cell-var-thin-hovered")
        case Some(Filler(_, _)) => getStyleClass.add("expr-cell-filler-hovered")
        case Some(FillerTarget(_, _, false)) => getStyleClass.add("expr-cell-filler-tgt-hovered")
        case Some(FillerTarget(_, _, true)) => getStyleClass.add("expr-cell-filler-tgt-thin-hovered")
      }
    }

    override def doUnhover = {
      item match {
        case None => getStyleClass.remove("expr-cell-empty-hovered")
        case Some(Variable(_, false)) => getStyleClass.remove("expr-cell-var-hovered")
        case Some(Variable(_, true)) => getStyleClass.remove("expr-cell-var-thin-hovered")
        case Some(Filler(_, _)) => getStyleClass.remove("expr-cell-filler-hovered")
        case Some(FillerTarget(_, _, false)) => getStyleClass.remove("expr-cell-filler-tgt-hovered")
        case Some(FillerTarget(_, _, true)) => getStyleClass.remove("expr-cell-filler-tgt-thin-hovered")
      }
    }

    override def doSelect = {
      item match {
        case None => getStyleClass.add("expr-cell-empty-selected")
        case Some(Variable(_, false)) => getStyleClass.add("expr-cell-var-selected")
        case Some(Variable(_, true)) => getStyleClass.add("expr-cell-var-thin-selected")
        case Some(Filler(_, _)) => getStyleClass.add("expr-cell-filler-selected")
        case Some(FillerTarget(_, _, false)) => getStyleClass.add("expr-cell-filler-tgt-selected")
        case Some(FillerTarget(_, _, true)) => getStyleClass.add("expr-cell-filler-tgt-thin-selected")
      }
    }

    override def doDeselect = {
      item match {
        case None => getStyleClass.remove("expr-cell-empty-selected")
        case Some(Variable(_, false)) => getStyleClass.remove("expr-cell-var-selected")
        case Some(Variable(_, true)) => getStyleClass.remove("expr-cell-var-thin-selected")
        case Some(Filler(_, _)) => getStyleClass.remove("expr-cell-filler-selected")
        case Some(FillerTarget(_, _, false)) => getStyleClass.remove("expr-cell-filler-tgt-selected")
        case Some(FillerTarget(_, _, true)) => getStyleClass.remove("expr-cell-filler-tgt-thin-selected")
      }
    }

  }

  class FrameworkEdge(owner : complex.CellType) extends JavaFXEdge(owner) {

    override def doHover : Unit = setStroke(Color.RED)
    override def doSelect : Unit = setStroke(Color.RED)
    override def doUnhover : Unit = setStroke(Color.BLACK)
    override def doDeselect : Unit = setStroke(Color.BLACK)

  }

  //============================================================================================
  // INITIALIZATION
  //

  var baseCell : FrameworkCell = newCell(complex.baseCells(baseIndex))

  refreshPanelData
  initializeChildren

}
