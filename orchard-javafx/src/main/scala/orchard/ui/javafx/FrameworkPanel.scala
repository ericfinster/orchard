/**
  * FrameworkPanel.scala - A Trait for panels which display expression information
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
import javafx.scene.{layout => jfxsl}

trait FrameworkPanel extends JavaFXPanel[Option[Expression]] { thisPanel : jfxsl.Region => 

  override type CellType <: FrameworkCell
  override type EdgeType <: FrameworkEdge

  override type ComplexType <: SimpleFramework

  abstract class FrameworkCell(owner : complex.CellType) extends JavaFXCell(owner) { thisCell : CellType =>

    def renderLabel : Node = {
      val labelNode = 
        item match {
          case None => new Region { prefWidth = 10 ; prefHeight = 10 }
          case Some(expr) => new Text(expr.id)
        }

      labelNode.layoutBounds onChange { thisPanel.refresh }

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

    //============================================================================================
    // EVENTS
    //

    override def onEventEmitted(ev : CellEvent) =
      ev match {
        case CellEntered(cell) => { owner.emitToFaces(RequestCellHovered) ; owner.emit(RequestEdgeHovered) }
        case CellExited(cell) => { owner.emitToFaces(RequestCellUnhovered) ; owner.emit(RequestEdgeUnhovered) }
        case _ => super.onEventEmitted(ev)
      }
  }

  abstract class FrameworkEdge(owner : complex.CellType) extends JavaFXEdge(owner) { thisEdge : EdgeType =>

    override def doHover : Unit = setStroke(Color.RED)
    override def doSelect : Unit = setStroke(Color.RED)
    override def doUnhover : Unit = setStroke(Color.BLACK)
    override def doDeselect : Unit = setStroke(Color.BLACK)

  }

}

