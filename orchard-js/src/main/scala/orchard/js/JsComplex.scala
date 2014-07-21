/**
  * JsComplex.scala - A Client Side Complex
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.js

import scala.language.existentials
import scala.collection.mutable.ListBuffer

import orchard.core.cell._
import orchard.core.util._
import orchard.core.complex._

import scala.scalajs._
import org.scalajs.dom
import dom.document
import org.scalajs.jquery.jQuery

class JsComplex[A] extends RenderableComplex[A] {

  type CellType = JsCell

  type PanelType = JsPanel

  type PanelCellType = JsPanelCell
  type PanelEdgeType = JsPanelEdge

  // This is temorary. We should control the topCell better ...
  var topCell : JsCell = null
  def newCell(item : A) : JsCell = new JsCell(item)

  var panels : List[Panel] = List()

  def generatePanels : Unit = {
    panels = 
      (for {
        i <- Range(0, dimension + 1)
      } yield { new JsPanel(i) }).toList
  }

  def renderAll : Unit = 
    panels foreach (_.render)

  class JsCell(val item : A) extends GalleryCell { thisCell =>

    var canopy : Option[RoseTree[JsCell, Int]] = None
    var target : Option[JsCell] = None
    var sources : Option[Vector[JsCell]] = None
    var container : Option[JsCell] = None
    
    var incoming : Option[JsCell] = None
    var outgoing : Option[JsCell] = None

    object MyPanelCell extends JsPanelCell {
      def complexCell = thisCell
    }

    object MyPanelEdge extends JsPanelEdge {
      def complexCell = thisCell
    }

    def panelCell = MyPanelCell
    def panelEdge = MyPanelEdge

    def faces : Array[CellType] = ???
    def neighborhood : Array[CellType] = ???

  }

  class JsPanel(index : Int) extends RenderablePanel {

    val svgNS = "http://www.w3.org/2000/svg"

    // Umm.  So we need to create some elements here and then
    // use snap to create svg surfaces on them.
    def panelId = "panel-" ++ index.toString

    val d = document.createElementNS(svgNS, "svg")

    jQuery(d).attr(js.Dynamic.literal(
      ("id" -> panelId),
      ("width" -> 0),
      ("height" -> 0)
    )).appendTo("#gallery")

    val paper = Snap("#" ++ panelId)

    override def render : Unit = {

      // Go through the cells and the edges and have them draw their elements
      val baseCellGroup = baseCell.cellGroup(paper)

      // Now make the actual rendering pass
      super.render

      baseCell foreachCell (cell => {
        cell.setDimensions
      })

      val viewBoxString = 
        panelX.toString ++ " " ++ 
          panelY.toString ++ " " ++ 
          panelWidth.toString ++ " " ++ 
          panelHeight.toString

      paper.attr(js.Dynamic.literal(
        "width" -> (panelWidth + 10).toString,
        "height" -> panelHeight,
        "viewBox" -> viewBoxString
      ))

    }

    override def clearRenderState : Unit = {
      super.clearRenderState
      paper.clear
    }

    def baseCell = baseCells(index).panelCell

  }

  trait JsPanelCell extends RenderableCell { thisCell =>

    var rectElement : Option[Element] = None
    var labelElement : Option[Element] = None

    def labelWidth : Double = (labelElement map (_.getBBox().width)) getOrElse 0.0
    def labelHeight : Double = (labelElement map (_.getBBox().height)) getOrElse 0.0

    def cellGroup(p : Paper) : Element = {
      val label = p.text(0, 0, item.toString)
      val rect = p.rect(0, 0, 0, 0, 4, 4)
      val group = p.g(new js.Array)

      rectElement = Some(rect)
      labelElement = Some(label)

      group.append(rect)
      group.append(label)

      for {
        tree <- canopy
      } {
        tree foreachCell (cell => {
          group.append(cell.cellGroup(p))
        })
      }

      group
    }

    def setDimensions : Unit = {
      for {
        rect <- rectElement
        label <- labelElement
      } {

        rect.attr(js.Dynamic.literal(
          "x" -> thisCell.x,
          "y" -> thisCell.y,
          "width" -> thisCell.width,
          "height" -> thisCell.height,
          "fill" -> "#FFF",
          "stroke" -> "#000",
          "strokeWidth" -> 2
        ))

        label.attr(js.Dynamic.literal(
          "x" -> thisCell.labelX,
          "y" -> thisCell.labelY
        ))

      }
    }

    override def clearRenderState : Unit = {
      rectElement = None
      labelElement = None
    }

  }

  trait JsPanelEdge extends RenderableEdge {

    def pathElement(p : Paper) : Element = {
      p.path("")
    }

  }

}
