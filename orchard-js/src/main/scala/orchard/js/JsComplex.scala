/**
  * JsComplex.scala - A Client Side Complex
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.js

import scala.language.existentials
import scala.collection.mutable.ListBuffer

import scala.scalajs._
import org.scalajs.dom
import dom.document

import org.scalajs.jquery.jQuery

import orchard.core.cell._
import orchard.core.util._
import orchard.core.complex._

import orchard.js.plugins._

abstract class JsComplex[A](json : js.Any)(implicit aReader : JsonReadable[A, js.Any])
    extends RenderableComplex[A] { thisComplex =>

  type CellType <: JsCell

  type PanelType <: JsPanel

  type PanelCellType <: JsPanelCell
  type PanelEdgeType <: JsPanelEdge

  val svgNS = "http://www.w3.org/2000/svg"

  def panelPadding = externalPadding * 2
  def newPanel(i : Int) : PanelType

  var remoteId : Int = 0
  var topCell : CellType = fromJson(json, JsJsonReader, aReader)

  def generatePanels : Unit =
    for {
      (base, i) <- baseCells.zipWithIndex
    } { base.panel = Some(newPanel(i)) }

  def clearPanels : Unit = 
    for {
      base <- baseCells
    } { base.panel = None }

  def renderAll : Unit = 
    for {
      base <- baseCells
      panel <- base.panel
    } { panel.render }

  //============================================================================================
  // JS CELL IMPLEMENTATION
  //

  abstract class JsCell extends GalleryCell { thisCell : CellType =>

    var canopy : Option[RoseTree[CellType, Int]] = None
    var target : Option[CellType] = None
    var sources : Option[Vector[CellType]] = None
    var container : Option[CellType] = None
    
    var incoming : Option[CellType] = None
    var outgoing : Option[CellType] = None

    var panel : Option[PanelType] = None

    var faces : List[CellType] = Nil

    def neighborhood : List[CellType] = ???

  }

  abstract class JsPanel(index : Int) extends RenderablePanel { thisPanel : PanelType =>

    var paperElement : Option[Paper] = None

    def renderContent(container : dom.Element) : Unit = {

      val svg = document.createElementNS(svgNS, "svg")

      val paper = Snap(svg)
      paperElement = Some(paper)

      // Add the cell content
      baseCell.renderContent(paper)
      container.appendChild(svg)

    }

    override def render : Unit = {

      // Now make the actual rendering pass
      super.render

      baseCell foreachCell (cell => {
        cell.setDimensions
      })

      // Add the edges
      for { tgt <- baseCell.target } {
        tgt foreachEdge (edge => {
          edge.renderPath
        })
      }
      
    }

    override def clearRenderState : Unit = {
      super.clearRenderState
      //paper.clear
    }

    def baseCell = baseCells(index).panelCell

  }

  trait JsPanelCell extends RenderableCell { thisCell : PanelCellType =>

    def styleBase : String = "orchard-cell"

    var isStyleHovered : Boolean = false
    var isStyleSelected : Boolean = false

    var groupElement : Option[Element] = None
    var rectElement : Option[Element] = None
    var labelElement : Option[Element] = None

    def setRectStyle : Unit = 
      for {
        rect <- rectElement
      } {
        val classString = 
          styleBase ++
            (if (isStyleHovered) " " ++ styleBase ++ "-hovered" else "") ++
            (if (isStyleSelected) " " ++ styleBase ++ "-selected" else "")

        rect.attr(js.Dynamic.literal(("class" -> classString)))
      }

    def labelWidth : Double = (labelElement map (_.node.getBoundingClientRect().width)) getOrElse 0.0
    def labelHeight : Double = (labelElement map (_.node.getBoundingClientRect().height)) getOrElse 0.0

    def requestHoveredStyle : Unit = {
      isStyleHovered = true
      setRectStyle
    }

    def requestUnhoveredStyle : Unit = {
      isStyleHovered = false
      setRectStyle
    }

    def requestSelectedStyle : Unit = {
      isStyleSelected = true
      setRectStyle
    }

    def requestUnselectedStyle : Unit = {
      isStyleSelected = false
      setRectStyle
    }

    def onMouseOver : Unit = for { f <- complexCell.faces } { f.panelCell.requestHoveredStyle }
    def onMouseOut : Unit = for { f <- complexCell.faces } { f.panelCell.requestUnhoveredStyle }
    def onMouseClick(e : js.Any) : Unit = ()
    def onMouseDoubleClick : Unit = ()

    def drawLabel(p : Paper) : Element = {
      val label = p.text(0, 0, item.toString)
      label.attr(js.Dynamic.literal(("class" -> "orchard-label")))
      labelElement = Some(label)
      label
    }

    def drawRect(p : Paper) : Element = {
      val rect = p.rect(0, 0, 0, 0, 4, 4)

      // Setup Mouse Event Callbacks
      rect.mouseover((() => { onMouseOver }) : js.Function)
      rect.mouseout((() => { onMouseOut }) : js.Function)
      rect.click(((e : js.Any) => { onMouseClick(e) }) : js.Function1[js.Any, Unit])
      rect.dblclick((() => { onMouseDoubleClick }) : js.Function)

      rectElement = Some(rect)
      setRectStyle

      rect
    }

    def renderContent(p : Paper) : Unit = {
      val group = p.g(new js.Array)
      val rect = drawRect(p)
      val label = drawLabel(p)

      group.append(rect)
      group.append(label)
      groupElement = Some(group)

      for {
        tree <- canopy
      } {
        tree foreachCell (cell => {
          cell.renderContent(p)
          cell.groupElement foreach (group.append(_))
        })
      }

      if (isBase) {
        // Add the edge content
        for { tgt <- target } {
          tgt foreachEdge (edge => {
            edge.renderContent(p)
            edge.pathElement foreach (group.append(_))
          })
        }
      }

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
          "stroke" -> "#000",
          "strokeWidth" -> "2px"
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

  trait JsPanelEdge extends RenderableEdge { thisEdge : PanelEdgeType => 

    var pathElement : Option[Element] = None

    def renderContent(p : Paper) : Element = {
      val path = p.path("")
      path.attr(js.Dynamic.literal(
        ("class" -> "orchard-edge"),
        ("stroke" -> "#000"),
        ("strokeWidth" -> "2px"),
        ("fill" -> "none")
      ))
      pathElement = Some(path)
      path
    }

    def renderPath : Unit = 
      for {
        path <- pathElement
      } {
        var pathString = "M " ++ incomingX.toString ++ " " ++ incomingY.toString ++ " "

        if (isVertical) {
          pathString ++= "V " ++ outgoingY.toString
        } else {
          pathString ++= "V " ++ (outgoingY - arcRadius).toString ++ " "
          pathString ++= "A " ++ arcRadius.toString ++ " " ++ arcRadius.toString ++ " 0 0 " ++ (if (incomingX > outgoingX) "1 " else "0 ") ++
            (if (incomingX > outgoingX) (incomingX - arcRadius) else (incomingX + arcRadius)).toString ++ " " ++ outgoingY.toString ++ " "
          pathString ++= "H " ++ outgoingX.toString
        }

      path.attr(js.Dynamic.literal(("d" -> pathString)))
    }
  }

  //============================================================================================
  // SERIALIZATION OVERRIDES
  //

  override def cellFromDescriptor(cell : CellType, desc : CellDescriptor, cellMap : Map[Int, CellType]) : Unit = {
    super.cellFromDescriptor(cell, desc, cellMap)
    cell.faces = desc.faces map (cellMap(_))
  }

  override def fromJson[P](
    x : P,
    reader : JsonReader[P],
    aReader : JsonReadable[A, P]
  ) : CellType = {
    val cell = super.fromJson(x, reader,aReader)
    // Grab the remote id as well
    remoteId = reader.readObjectField(x, "complex").asInstanceOf[js.Number].toInt
    cell
  }

}
