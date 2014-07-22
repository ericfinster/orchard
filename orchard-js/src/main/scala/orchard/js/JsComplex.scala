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

class JsComplex[A](
  container : dom.Element, 
  json : js.Any,
  svgPanelWidth : Int, 
  svgPanelHeight : Int, 
  displayPanels : Int
)(implicit aReader : JsonReadable[A, js.Any])
    extends RenderableComplex[A] { thisComplex =>

  type CellType = JsCell

  type PanelType = JsPanel

  type PanelCellType = JsPanelCell
  type PanelEdgeType = JsPanelEdge

  def panelPadding = externalPadding * 2

  def newCell(item : A) : JsCell = new JsCell(item)
  var topCell : JsCell = fromJson(json, JsJsonReader, aReader)

  def generatePanels : Unit =
    for {
      (base, i) <- baseCells.zipWithIndex
    } { base.panel = Some(new JsPanel(i)) }

  def clearPanels : Unit = 
    for {
      base <- baseCells
    } { base.panel = None }

  def renderAll : Unit = 
    for {
      base <- baseCells
      panel <- base.panel
    } { panel.render }

  // Now, I want to add some carousel type features.  How is that going to look?

  def initializeContent : Unit = {

    val carousel = document.createElement("div")
    carousel.setAttribute("id", "gallery")
    carousel.setAttribute("class", "jcarousel")
    carousel.style.width = ((svgPanelWidth * displayPanels).toString ++ "px")
    carousel.style.height = (svgPanelHeight.toString ++ "px")
    container.appendChild(carousel)

    val carouselList = document.createElement("ul")
    carousel.appendChild(carouselList)

    // Create the panels
    generatePanels

    for {
      base <- baseCells
      panel <- base.panel
    } { 
      carouselList.appendChild(panel.getContent)
    }

    // Generate the controls
    val carouselPrev = document.createElement("a")
    carouselPrev.setAttribute("class", "jcarousel-prev")
    carouselPrev.setAttribute("href", "#")
    carouselPrev.appendChild(document.createTextNode("Prev"))
    container.appendChild(carouselPrev)

    val carouselNext = document.createElement("a")
    carouselNext.setAttribute("class", "jcarousel-next")
    carouselNext.setAttribute("href", "#")
    carouselNext.appendChild(document.createTextNode("Next"))
    container.appendChild(carouselNext)

  }

  initializeContent

  //============================================================================================
  // JS CELL IMPLEMENTATION
  //

  class JsCell(val item : A) extends GalleryCell { thisCell =>

    var canopy : Option[RoseTree[JsCell, Int]] = None
    var target : Option[JsCell] = None
    var sources : Option[Vector[JsCell]] = None
    var container : Option[JsCell] = None
    
    var incoming : Option[JsCell] = None
    var outgoing : Option[JsCell] = None

    var panel : Option[JsPanel] = None

    var faces : List[JsCell] = Nil

    object MyPanelCell extends JsPanelCell {
      def complexCell = thisCell
    }

    object MyPanelEdge extends JsPanelEdge {
      def complexCell = thisCell
    }

    def panelCell = MyPanelCell
    def panelEdge = MyPanelEdge

    def neighborhood : List[CellType] = ???

  }

  class JsPanel(index : Int) extends RenderablePanel {

    def panelId = "panel-" ++ index.toString

    var paperElement : Option[Paper] = None

    def getContent : dom.Element = {

      val svgNS = "http://www.w3.org/2000/svg"

      val svg = document.createElementNS(svgNS, "svg")

      val li = document.createElement("li")
      li.style.width = svgPanelWidth.toString ++ "px"
      li.style.height = svgPanelHeight.toString ++ "px"
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
          "width" -> (svgPanelWidth - (2 * panelPadding)).toString,
          "height" -> (svgPanelHeight - (2 * panelPadding)).toString,
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

  trait JsPanelCell extends RenderableCell { thisCell =>

    var groupElement : Option[Element] = None
    var rectElement : Option[Element] = None
    var labelElement : Option[Element] = None

    def labelWidth : Double = (labelElement map (_.getBBox().width)) getOrElse 0.0
    def labelHeight : Double = (labelElement map (_.getBBox().height)) getOrElse 0.0

    def requestHovered : Unit = 
      for {
        r <- rectElement
      } {
        r.attr(js.Dynamic.literal(( "fill" -> "#f34")))
      }

    def requestUnhovered : Unit = 
      for {
        r <- rectElement
      } {
        r.attr(js.Dynamic.literal(( "fill" -> "#fff")))
      }

    def requestSelected : Unit = ()
    def requestUnselected : Unit = ()

    def drawCellContent(p : Paper) : Element = {
      val label = p.text(0, 0, item.toString)
      val rect = p.rect(0, 0, 0, 0, 4, 4)
      val group = p.g(new js.Array)

      // Right.  Here we want to set up some callbacks
      rect.mouseover((() => {
        for { f <- complexCell.faces } { f.panelCell.requestHovered }
      }) : js.Function)

      rect.mouseout((() => {
        for { f <- complexCell.faces } { f.panelCell.requestUnhovered }
      }) : js.Function)

      rectElement = Some(rect)
      labelElement = Some(label)

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
          "fill" -> "#FFF",
          "stroke" -> "#000",
          "strokeWidth" -> 2
        ))

        label.attr(js.Dynamic.literal(
          "x" -> thisCell.labelX,
          "y" -> thisCell.labelY,
          "pointer-events" -> "none"
        ))

      }
    }

    override def clearRenderState : Unit = {
      rectElement = None
      labelElement = None
    }

  }

  trait JsPanelEdge extends RenderableEdge {

    var pathElement : Option[Element] = None

    def drawPath(p : Paper) : Element = {
      val path = p.path("")
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

      path.attr(js.Dynamic.literal(
        ("d" -> pathString),
        ("stroke" -> "#000"),
        ("strokeWidth" -> 2),
        ("fill" -> "none"),
        ("pointer-events" -> "none")
      ))
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
