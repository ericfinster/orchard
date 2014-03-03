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

  override def refresh = {
    super.refresh
    baseCell foreachCell (cell => cell.assignStyle)
  }

  abstract class FrameworkCell(owner : complex.CellType) extends JavaFXCell(owner) { thisCell : CellType =>

    def isExposedStyle : Boolean = {
      val outgoingIsNook =
        owner.outgoing match {
          case None => false
          case Some(c) => {
            c.isExposedNook
          }
        }

      val incomingIsNook = 
        owner.incoming match {
          case None => false
          case Some(c) => {
            c.isExposedNook
          }
        }

      owner.isExposedNook || outgoingIsNook || incomingIsNook
    }

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

    var lastStyle : Option[String] = None

    def setCellStyle(style : String) = {
      lastStyle foreach (s => getStyleClass.remove(s))
      getStyleClass.add(style)
      lastStyle = Some(style)
    }

    def assignStyle = 
      item match {
        case None => {
          if (isExposedStyle) {
            setCellStyle("expr-cell-exposed")
          } else {
            setCellStyle("expr-cell-empty")
          }
        }
        case Some(Variable(_, false)) => setCellStyle("expr-cell-var")
        case Some(Variable(_, true)) => setCellStyle("expr-cell-var-thin")
        case Some(Filler(_)) => setCellStyle("expr-cell-filler")
        case Some(FillerFace(_, _, false)) => setCellStyle("expr-cell-filler-face")
        case Some(FillerFace(_, _, true)) => setCellStyle("expr-cell-filler-face-thin")
        case Some(UnicityFiller(_)) => setCellStyle("expr-cell-ufiller")
      }

    assignStyle

    //============================================================================================
    // HOVER AND SELECTION
    //

    override def doHover = {
      item match {
        case None => {
          if (isExposedStyle) {
            getStyleClass.add("expr-cell-exposed-hovered")
          } else {
            getStyleClass.add("expr-cell-empty-hovered")
          }
        }
        case Some(Variable(_, false)) => getStyleClass.add("expr-cell-var-hovered")
        case Some(Variable(_, true)) => getStyleClass.add("expr-cell-var-thin-hovered")
        case Some(Filler(_)) => getStyleClass.add("expr-cell-filler-hovered")
        case Some(FillerFace(_, _, false)) => getStyleClass.add("expr-cell-filler-face-hovered")
        case Some(FillerFace(_, _, true)) => getStyleClass.add("expr-cell-filler-face-thin-hovered")
        case Some(UnicityFiller(_)) => getStyleClass.add("expr-cell-ufiller-hovered")
      }
    }

    override def doUnhover = {
      item match {
        case None => {
          if (isExposedStyle) {
            getStyleClass.remove("expr-cell-exposed-hovered")
          } else {
            getStyleClass.remove("expr-cell-empty-hovered")
          }
        }
        case Some(Variable(_, false)) => getStyleClass.remove("expr-cell-var-hovered")
        case Some(Variable(_, true)) => getStyleClass.remove("expr-cell-var-thin-hovered")
        case Some(Filler(_)) => getStyleClass.remove("expr-cell-filler-hovered")
        case Some(FillerFace(_, _, false)) => getStyleClass.remove("expr-cell-filler-face-hovered")
        case Some(FillerFace(_, _, true)) => getStyleClass.remove("expr-cell-filler-face-thin-hovered")
        case Some(UnicityFiller(_)) => getStyleClass.remove("expr-cell-ufiller-hovered")
      }
    }

    override def doSelect = {
      item match {
        case None => {
          if (isExposedStyle) {
            getStyleClass.add("expr-cell-exposed-selected")
          } else {
            getStyleClass.add("expr-cell-empty-selected")
          }
        }
        case Some(Variable(_, false)) => getStyleClass.add("expr-cell-var-selected")
        case Some(Variable(_, true)) => getStyleClass.add("expr-cell-var-thin-selected")
        case Some(Filler(_)) => getStyleClass.add("expr-cell-filler-selected")
        case Some(FillerFace(_, _, false)) => getStyleClass.add("expr-cell-filler-face-selected")
        case Some(FillerFace(_, _, true)) => getStyleClass.add("expr-cell-filler-face-thin-selected")
        case Some(UnicityFiller(_)) => getStyleClass.add("expr-cell-ufiller-selected")
      }
    }

    override def doDeselect = {
      item match {
        case None => {
          if (isExposedStyle) {
            getStyleClass.remove("expr-cell-exposed-selected")
          } else {
            getStyleClass.remove("expr-cell-empty-selected")
          }
        }
        case Some(Variable(_, false)) => getStyleClass.remove("expr-cell-var-selected")
        case Some(Variable(_, true)) => getStyleClass.remove("expr-cell-var-thin-selected")
        case Some(Filler(_)) => getStyleClass.remove("expr-cell-filler-selected")
        case Some(FillerFace(_, _, false)) => getStyleClass.remove("expr-cell-filler-face-selected")
        case Some(FillerFace(_, _, true)) => getStyleClass.remove("expr-cell-filler-face-thin-selected")
        case Some(UnicityFiller(_)) => getStyleClass.remove("expr-cell-ufiller-selected")
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

    override def doHover : Unit = setStroke(Color.TOMATO)
    override def doSelect : Unit = setStroke(Color.TOMATO)
    override def doUnhover : Unit = setStroke(Color.BLACK)
    override def doDeselect : Unit = setStroke(Color.BLACK)

  }

}

