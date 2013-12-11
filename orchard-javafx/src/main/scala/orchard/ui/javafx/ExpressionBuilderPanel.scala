/**
  * ExpressionBuilderPanel.scala - A panel and visual cell type for editing expressions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.Includes._

import scalafx.scene.Node
import scalafx.scene.text.Text
import scalafx.scene.layout.Region
import scalafx.scene.effect.ColorAdjust

import scalafx.scene.paint.Color

import scalafx.geometry.Bounds

import javafx.{scene => jfxs}

import orchard.core._

class ExpressionBuilderPanel(val complex : ExpressionBuilderComplex, baseIndex : Int)
    extends JavaFXPanel[Polarity[Option[Expression]]] 
    with MutablePanel[Polarity[Option[Expression]]] { thisPanel =>

  type CellType = ExpressionBuilderCell
  type EdgeType = ExpressionBuilderEdge

  type ComplexType = ExpressionBuilderComplex
  type LabelType = Polarity[Option[Expression]]

  class ExpressionBuilderCell(owner : complex.ExpressionBuilderCell) extends JavaFXCell(owner) with MutablePanelCell {

    //============================================================================================
    // INITIALIZATION
    //

    getStyleClass().add("expression-cell")

    def renderCell = {
      assignStyle
      label = renderLabel
    }

    def renderLabel : jfxs.Node = {
      val labelNode = 
        item match {
          case Positive => new Text("+")
          case Negative => new Text("-")
          case Neutral(None) => new Region { prefWidth = 10 ; prefHeight = 10 }
          case Neutral(Some(expr)) => new Text(expr.id)
        }

      labelNode.layoutBounds onChange { thisPanel.refresh }

      pane.getChildren.setAll(labelNode)
      labelNode
    }

    def assignStyle = 
      item match {
        case Positive => getStyleClass.add("expr-cell-polarized")
        case Negative => getStyleClass.add("expr-cell-polarized")
        case Neutral(None) => {
          getStyleClass.add("expr-cell-empty")

          // if (owner.isExposedNook) {
          //   getStyleClass.add("expr-cell-exposed-nook")
          // } else {
          //   getStyleClass.add("expr-cell-empty")
          // }
        }
        case Neutral(Some(Variable(_, false))) => getStyleClass.add("expr-cell-var")
        case Neutral(Some(Variable(_, true))) => getStyleClass.add("expr-cell-var-thin")
        case Neutral(Some(Filler(_, _))) => getStyleClass.add("expr-cell-filler")
        case Neutral(Some(FillerTarget(_, _, false))) => getStyleClass.add("expr-cell-filler-tgt")
        case Neutral(Some(FillerTarget(_, _, true))) => getStyleClass.add("expr-cell-filler-tgt-thin")
      }

    assignStyle

    //============================================================================================
    // HOVER AND SELECTION
    //

    override def doHover = {
      item match {
        case Positive => () 
        case Negative => () 
        case Neutral(None) => getStyleClass.add("expr-cell-empty-hovered")
        case Neutral(Some(Variable(_, false))) => getStyleClass.add("expr-cell-var-hovered")
        case Neutral(Some(Variable(_, true))) => getStyleClass.add("expr-cell-var-thin-hovered")
        case Neutral(Some(Filler(_, _))) => getStyleClass.add("expr-cell-filler-hovered")
        case Neutral(Some(FillerTarget(_, _, false))) => getStyleClass.add("expr-cell-filler-tgt-hovered")
        case Neutral(Some(FillerTarget(_, _, true))) => getStyleClass.add("expr-cell-filler-tgt-thin-hovered")
      }
    }

    override def doUnhover = {
      item match {
        case Positive => () 
        case Negative => () 
        case Neutral(None) => getStyleClass.remove("expr-cell-empty-hovered")
        case Neutral(Some(Variable(_, false))) => getStyleClass.remove("expr-cell-var-hovered")
        case Neutral(Some(Variable(_, true))) => getStyleClass.remove("expr-cell-var-thin-hovered")
        case Neutral(Some(Filler(_, _))) => getStyleClass.remove("expr-cell-filler-hovered")
        case Neutral(Some(FillerTarget(_, _, false))) => getStyleClass.remove("expr-cell-filler-tgt-hovered")
        case Neutral(Some(FillerTarget(_, _, true))) => getStyleClass.remove("expr-cell-filler-tgt-thin-hovered")
      }
    }

    override def doSelect = {
      item match {
        case Positive => ()
        case Negative => ()
        case Neutral(None) => getStyleClass.add("expr-cell-empty-selected")
        case Neutral(Some(Variable(_, false))) => getStyleClass.add("expr-cell-var-selected")
        case Neutral(Some(Variable(_, true))) => getStyleClass.add("expr-cell-var-thin-selected")
        case Neutral(Some(Filler(_, _))) => getStyleClass.add("expr-cell-filler-selected")
        case Neutral(Some(FillerTarget(_, _, false))) => getStyleClass.add("expr-cell-filler-tgt-selected")
        case Neutral(Some(FillerTarget(_, _, true))) => getStyleClass.add("expr-cell-filler-tgt-thin-selected")
      }
    }

    override def doDeselect = {
      item match {
        case Positive => ()
        case Negative => ()
        case Neutral(None) => getStyleClass.remove("expr-cell-empty-selected")
        case Neutral(Some(Variable(_, false))) => getStyleClass.remove("expr-cell-var-selected")
        case Neutral(Some(Variable(_, true))) => getStyleClass.remove("expr-cell-var-thin-selected")
        case Neutral(Some(Filler(_, _))) => getStyleClass.remove("expr-cell-filler-selected")
        case Neutral(Some(FillerTarget(_, _, false))) => getStyleClass.remove("expr-cell-filler-tgt-selected")
        case Neutral(Some(FillerTarget(_, _, true))) => getStyleClass.remove("expr-cell-filler-tgt-thin-selected")
      }
    }

    //============================================================================================
    // EVENTS
    //

    // Dispatch mutability events ... I think this event system
    // needs to be entirely reworked.  It's getting pretty messy ...
    override def onEventEmitted(ev : CellEvent) = {
      if (ev.isInstanceOf[complex.ChangeEvents.ChangeEvent]) {
        ev match {
          case complex.ChangeEvents.ItemChangedEvent(oldItem) => {
            println("Item has changed: " ++ item.toString)
            renderCell
            refresh
          }
          case _ => onCellChangeEvent(ev.asInstanceOf[complex.ChangeEvents.ChangeEvent])
        }
      } else {
        ev match {
          case CellEntered(cell) => if (owner.isPolarized) () else owner.emitToFaces(RequestHovered)
          case CellExited(cell) => if (owner.isPolarized) () else owner.emitToFaces(RequestUnhovered)
          case _ => super.onEventEmitted(ev)
        }
      }
    }
  }

  class ExpressionBuilderEdge(owner : complex.ExpressionBuilderCell) extends JavaFXEdge(owner) with MutablePanelEdge {

    override def doHover : Unit = setStroke(Color.RED)
    override def doSelect : Unit = setStroke(Color.RED)
    override def doUnhover : Unit = setStroke(Color.BLACK)
    override def doDeselect : Unit = setStroke(Color.BLACK)
    
  }

  def newCell(owner : complex.ExpressionBuilderCell) : ExpressionBuilderCell = { 
    val cell = new ExpressionBuilderCell(owner)
    reactTo(cell) 
    cell 
  }
  
  def newEdge(owner : complex.ExpressionBuilderCell) : ExpressionBuilderEdge = { 
    val edge = new ExpressionBuilderEdge(owner) 
    reactTo(edge) 
    edge 
  }

  //============================================================================================
  // INITIALIZATION
  //

  var baseCell : ExpressionBuilderCell = {
    val seed = complex.baseCells(baseIndex)
    generatePanelData(seed, for { srcs <- seed.sources } yield (srcs map (src => newEdge(src))))
  }

  initializeChildren

}
