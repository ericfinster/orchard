/**
  * JsCarouselComplex.scala - A Client Side Complex
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

abstract class JsCarouselComplex[A](json : js.Any)(implicit aReader : JsonReadable[A, js.Any])
    extends RenderableComplex[A] { thisComplex =>

  type CellType <: JsCell

  type PanelType <: JsPanel

  type PanelCellType <: JsPanelCell
  type PanelEdgeType <: JsPanelEdge

  def container : dom.Element
  def galleryPanelWidth : Int
  def galleryPanelHeight : Int
  def displayPanels : Int

  def panelPadding = externalPadding * 2
  def newPanel(i : Int) : PanelType

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
  // INITIALIZATION
  //

  val galleryAssembly = document.createElement("div")
  galleryAssembly.setAttribute("class", "orchard-gallery-assembly")
  container.appendChild(galleryAssembly)

  val galleryWrapper = document.createElement("div")
  galleryWrapper.setAttribute("class", "orchard-gallery-wrapper")
  galleryAssembly.appendChild(galleryWrapper)

  val gallery = document.createElement("div")
  gallery.setAttribute("class", "orchard-gallery")
  galleryWrapper.appendChild(gallery)

  val galleryPanelList = document.createElement("ul")
  gallery.appendChild(galleryPanelList)

  // Generate the controls
  val galleryPrev = document.createElement("a")
  galleryPrev.setAttribute("class", "orchard-gallery-control-prev")
  galleryPrev.setAttribute("href", "#")
  galleryAssembly.appendChild(galleryPrev)

  val galleryPrevBtn = document.createElement("i")
  galleryPrevBtn.setAttribute("class", "fa fa-chevron-circle-left fa-lg")
  galleryPrev.appendChild(galleryPrevBtn)

  val galleryNext = document.createElement("a")
  galleryNext.setAttribute("class", "orchard-gallery-control-next")
  galleryNext.setAttribute("href", "#")
  galleryAssembly.appendChild(galleryNext)

  val galleryNextBtn = document.createElement("i")
  galleryNextBtn.setAttribute("class", "fa fa-chevron-circle-right fa-lg")
  galleryNext.appendChild(galleryNextBtn)

  val galleryPagination = document.createElement("p")
  galleryPagination.setAttribute("class", "orchard-gallery-pagination")
  galleryAssembly.appendChild(galleryPagination)

  initializeContent

  import plugins.JQueryCarousel._

  val carousel = jQuery(gallery).jcarousel()

  def initializeContent : Unit = {

    jQuery(galleryPanelList).empty()

    // Create the panels
    generatePanels

    for {
      base <- baseCells
      panel <- base.panel
    } { 
      galleryPanelList.appendChild(panel.getContent)
    }

    carousel.reload(js.Dynamic.literal())

  }

  def refreshFromJson(newJson : js.Any) : Unit = {

    topCell = fromJson(newJson, JsJsonReader, aReader)
    initializeContent
    renderAll

  }

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

  class JsPanel(index : Int) extends RenderablePanel { thisPanel : PanelType =>

    def panelId = "panel-" ++ index.toString

    var paperElement : Option[Paper] = None

    def getContent : dom.Element = {

      val svgNS = "http://www.w3.org/2000/svg"

      val svg = document.createElementNS(svgNS, "svg")

      val li = document.createElement("li")
      li.style.width = galleryPanelWidth.toString ++ "px"
      li.style.height = galleryPanelHeight.toString ++ "px"
      li.appendChild(svg)

      val paper = Snap(svg)
      paperElement = Some(paper)

      // Add the cell content
      baseCell.drawCellContent(paper)

      // Add the edge content
      for { tgt <- baseCell.target ; baseGroup <- baseCell.groupElement } {
        tgt foreachEdge (edge => {
          baseGroup.append(edge.drawPath(paper))
        })
      }

      li

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
      
      val viewBoxString = 
        panelX.toString ++ " " ++ 
          panelY.toString ++ " " ++ 
          panelWidth.toString ++ " " ++ 
          panelHeight.toString

      for {
        paper <- paperElement
      } {
        paper.attr(js.Dynamic.literal(
          "width" -> (galleryPanelWidth - (2 * panelPadding)).toString,
          "height" -> (galleryPanelHeight - (2 * panelPadding)).toString,
          "viewBox" -> viewBoxString
        ))
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

    def labelWidth : Double = (labelElement map (_.getBBox().width)) getOrElse 0.0
    def labelHeight : Double = (labelElement map (_.getBBox().height)) getOrElse 0.0

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

    def drawCellContent(p : Paper) : Element = {
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
          group.append(cell.drawCellContent(p))
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

    def drawPath(p : Paper) : Element = {
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
}
