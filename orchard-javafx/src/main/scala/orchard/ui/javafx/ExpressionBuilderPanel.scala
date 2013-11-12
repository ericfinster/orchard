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
        case Positive => getStyleClass.add("expression-cell-polarized")
        case Negative => getStyleClass.add("expression-cell-polarized")
        case Neutral(None) => getStyleClass.add("expression-cell-empty")
        case Neutral(Some(expr)) => {
          if (expr.isThin) 
            getStyleClass.add("expression-cell-thin")
          else
            getStyleClass.add("expression-cell-expr")
        }
      }

    assignStyle

    //============================================================================================
    // HOVER AND SELECTION
    //

    var isSelected : Boolean = false
    var isHovered : Boolean = false

    override def doHover = {
      isHovered = true

      item match {
        case Positive => () // getStyleClass.add("expression-cell-polarized-hovered")
        case Negative => () // getStyleClass.add("expression-cell-polarized-hovered")
        case Neutral(None) => getStyleClass.add("expression-cell-empty-hovered")
        case Neutral(Some(expr)) => getStyleClass.add("expression-cell-expr-hovered")
      }
    }

    override def doUnhover = {
      isHovered = false

      item match {
        case Positive => () // getStyleClass.remove("expression-cell-polarized-hovered")
        case Negative => () // getStyleClass.remove("expression-cell-polarized-hovered")
        case Neutral(None) => getStyleClass.remove("expression-cell-empty-hovered")
        case Neutral(Some(expr)) => getStyleClass.remove("expression-cell-expr-hovered")
      }
    }

    override def doSelect = {
      isSelected = true

      item match {
        case Positive => getStyleClass.add("expression-cell-polarized-selected")
        case Negative => getStyleClass.add("expression-cell-polarized-selected")
        case Neutral(None) => getStyleClass.add("expression-cell-empty-selected")
        case Neutral(Some(expr)) => getStyleClass.add("expression-cell-expr-selected")
      }

    }

    override def doDeselect = {
      isSelected = false

      item match {
        case Positive => getStyleClass.remove("expression-cell-polarized-selected")
        case Negative => getStyleClass.remove("expression-cell-polarized-selected")
        case Neutral(None) => getStyleClass.remove("expression-cell-empty-selected")
        case Neutral(Some(expr)) => getStyleClass.remove("expression-cell-expr-selected")
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
        super.onEventEmitted(ev)
      }
    }
  }

  class ExpressionBuilderEdge(owner : complex.ExpressionBuilderCell) extends JavaFXEdge(owner) with MutablePanelEdge

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
