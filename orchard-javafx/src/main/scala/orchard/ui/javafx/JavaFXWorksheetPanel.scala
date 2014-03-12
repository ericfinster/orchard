/**
  * JavaFXWorksheetPanel.scala - A worksheet panel in JavaFX
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.Includes._

import scalafx.scene.Node
import scalafx.scene.text.Text
import scalafx.scene.layout.Region
import scalafx.scene.paint.Color

import javafx.{scene => jfxs}

import orchard.core._

abstract class JavaFXWorksheetPanel
    extends ZoomPanel[Polarity[Option[Expression]]] 
    with MutablePanel[Polarity[Option[Expression]]] { thisPanel =>

  type CellType <: JavaFXWorksheetCell
  type EdgeType <: JavaFXWorksheetEdge

  type ComplexType <: Workspace#Worksheet
  type LabelType = Polarity[Option[Expression]]

  override def refresh = {
    super.refresh
    baseCell foreachCell (cell => cell.assignStyle)
  }

  abstract class JavaFXWorksheetCell extends JavaFXCell with MutablePanelCell { thisCell : CellType =>

    //============================================================================================
    // INITIALIZATION
    //

    val cellStyleIndex = getStyleClass.length
    val cellSelectedStyleIndex = cellStyleIndex + 1
    val cellHoveredStyleIndex = cellStyleIndex + 2

    getStyleClass add "expr-null"
    getStyleClass add "expr-selected-null"
    getStyleClass add "expr-hovered-null"

    def dumpStyle = {
      val styleClass = getStyleClass
      println("Dumpting style info: " ++ styleClass.toString)
    }

    def renderCell = {
      assignStyle
      label = renderLabel
    }

    def setCellStyle(style : String) = getStyleClass(cellStyleIndex) = style
    def setCellSelectedStyle(style : String) = getStyleClass(cellSelectedStyleIndex) = style
    def setCellHoveredStyle(style : String) = getStyleClass(cellHoveredStyleIndex) = style

    def removeCellStyle = setCellStyle("expr-null")
    def removeCellSelectedStyle = setCellSelectedStyle("expr-selected-null")
    def removeCellHoveredStyle = setCellHoveredStyle("expr-hovered-null")

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
 
    def isExposedStyle : Boolean = {
      if (owner.isExposedNook) return true

      val outgoingIsNook =
        owner.outgoing match {
          case None => false
          case Some(c) => {
            if (c.isPolarized) false else c.isExposedNook
          }
        }

      val incomingIsNook = 
        owner.incoming match {
          case None => false
          case Some(c) => {
            if (c.isPolarized) false else c.isExposedNook
          }
        }

      outgoingIsNook || incomingIsNook
    }

    def assignStyle =
      item match {
        case Positive => setCellStyle("expr-cell-polarized")
        case Negative => setCellStyle("expr-cell-polarized")
        case Neutral(None) => {
          if (isExposedStyle) {
            setCellStyle("expr-cell-exposed")
          } else {
            setCellStyle("expr-cell-empty")
          }
        }
        case Neutral(Some(Variable(_, false))) => setCellStyle("expr-cell-var")
        case Neutral(Some(Variable(_, true))) => setCellStyle("expr-cell-var-thin")
        case Neutral(Some(Filler(_))) => setCellStyle("expr-cell-filler")
        case Neutral(Some(FillerFace(_, _, false))) => setCellStyle("expr-cell-filler-face")
        case Neutral(Some(FillerFace(_, _, true))) => setCellStyle("expr-cell-filler-face-thin")
        case Neutral(Some(UnicityFiller(_))) => setCellStyle("expr-cell-ufiller")
        case Neutral(Some(Application(_, _, _))) => setCellStyle("expr-cell-app")
      }

    assignStyle

    //============================================================================================
    // HOVER AND SELECTION
    //

    override def doHover = {
      item match {
        case Positive => () 
        case Negative => () 
        case Neutral(None) => {
          if (isExposedStyle) {
            setCellHoveredStyle("expr-cell-exposed-hovered")
          } else {
            setCellHoveredStyle("expr-cell-empty-hovered")
          }
        }
        case Neutral(Some(Variable(_, false))) => setCellHoveredStyle("expr-cell-var-hovered")
        case Neutral(Some(Variable(_, true))) => setCellHoveredStyle("expr-cell-var-thin-hovered")
        case Neutral(Some(Filler(_))) => setCellHoveredStyle("expr-cell-filler-hovered")
        case Neutral(Some(FillerFace(_, _, false))) => setCellHoveredStyle("expr-cell-filler-face-hovered")
        case Neutral(Some(FillerFace(_, _, true))) => setCellHoveredStyle("expr-cell-filler-face-thin-hovered")
        case Neutral(Some(UnicityFiller(_))) => setCellHoveredStyle("expr-cell-ufiller-hovered")
        case Neutral(Some(Application(_, _, _))) => setCellHoveredStyle("expr-cell-app-hovered")
      }
    }

    override def doSelect = {
      item match {
        case Positive => ()
        case Negative => ()
        case Neutral(None) => {
          if (isExposedStyle) {
            setCellSelectedStyle("expr-cell-exposed-selected")
          } else {
            setCellSelectedStyle("expr-cell-empty-selected")
          }
        }
        case Neutral(Some(Variable(_, false))) => setCellSelectedStyle("expr-cell-var-selected")
        case Neutral(Some(Variable(_, true))) => setCellSelectedStyle("expr-cell-var-thin-selected")
        case Neutral(Some(Filler(_))) => setCellSelectedStyle("expr-cell-filler-selected")
        case Neutral(Some(FillerFace(_, _, false))) => setCellSelectedStyle("expr-cell-filler-face-selected")
        case Neutral(Some(FillerFace(_, _, true))) => setCellSelectedStyle("expr-cell-filler-face-thin-selected")
        case Neutral(Some(UnicityFiller(_))) => setCellSelectedStyle("expr-cell-ufiller-selected")
        case Neutral(Some(Application(_, _, _))) => setCellSelectedStyle("expr-cell-app-selected")          
      }
    }

    override def doUnhover = removeCellHoveredStyle
    override def doDeselect = removeCellSelectedStyle

    //============================================================================================
    // EVENTS
    //

    // Dispatch mutability events ... I think this event system
    // needs to be entirely reworked.  It's getting pretty messy ...
    override def onEventEmitted(ev : CellEvent) = {
      ev match {

        case complex.ChangeEvents.ItemChangedEvent(oldItem) => {
          renderCell

          // The rest of the gallery may need to be updated as well, so pass
          // this even along ...
          super.onEventEmitted(ev)
        }

        case CellEntered(cell) => if (owner.isPolarized) () else { owner.emitToFaces(RequestCellHovered) ; owner.emit(RequestEdgeHovered) }
        case CellExited(cell) => if (owner.isPolarized) () else { owner.emitToFaces(RequestCellUnhovered) ; owner.emit(RequestEdgeUnhovered) }
        case _ => super.onEventEmitted(ev)
      }
    }
  }

  abstract class JavaFXWorksheetEdge extends JavaFXEdge with MutablePanelEdge { thisEdge : EdgeType => 

    override def doHover : Unit = setStroke(Color.TOMATO)
    override def doSelect : Unit = setStroke(Color.TOMATO)
    override def doUnhover : Unit = setStroke(Color.BLACK)
    override def doDeselect : Unit = setStroke(Color.BLACK)
    
  }

}
