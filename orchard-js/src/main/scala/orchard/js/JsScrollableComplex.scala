/**
  * JsScrollableComplex.scala - A Scrollable Javascript Complex
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.js

import scala.scalajs._

import org.scalajs.dom
import org.scalajs.jquery._

import orchard.core.cell._
import orchard.core.util._
import orchard.core.complex._

import plugins.PerfectScrollbar._

abstract class JsScrollableComplex[A](container : dom.Element, json : js.Any, panelSize : Int)(implicit aReader : JsonReadable[A, js.Any])
    extends JsComplex[A](json) { thisComplex =>

  type PanelType = JsScrollablePanel

  //============================================================================================
  // INITIALIZATION
  //

  val galleryDiv = dom.document.createElement("div")
  galleryDiv.setAttribute("class", "scrollable-gallery")
  container.appendChild(galleryDiv)

  val galleryList = dom.document.createElement("div")
  galleryList.setAttribute("class", "scrollable-gallery-panels")
  galleryDiv.appendChild(galleryList)

  renderContent

  // jQuery(galleryDiv).perfectScrollbar()

  def renderContent : Unit = {

    // Create the panels
    generatePanels

    for {
      base <- baseCells
      panel <- base.panel
    } { 
      val panelDiv = dom.document.createElement("div")
      panelDiv.setAttribute("class", "scrollable-gallery-panel")
      galleryList.appendChild(panelDiv)
      panel.renderContent(panelDiv)
    }

    // Now, we need to set the size of the panels ul element to be large enough ...
    jQuery(galleryList).width(panelSize * baseCells.length)

  }

  def refreshFromJson(newJson : js.Any) : Unit = {
    clearPanels
    // Do this by hand????
    jQuery(galleryList).empty()
    topCell = fromJson(newJson, JsJsonReader, aReader)
    renderContent
    renderAll
    // jQuery(galleryDiv).perfectScrollbar("update")
  }

  //============================================================================================
  // PANEL IMPLEMENTATION
  //

  class JsScrollablePanel(index : Int) extends JsPanel(index) {

    override def render : Unit = {
      super.render
      setSvgSize(panelSize, panelSize)
    }

    def setSvgSize(svgWidth : Int, svgHeight : Int) : Unit = {

      val viewBoxX =
        if (panelWidth > svgWidth)
          panelX
        else {
          panelX - ((svgWidth - panelWidth) / 2)
        }

      val viewBoxY =
        if (panelHeight > svgHeight)
          panelY
        else {
          panelY - ((svgHeight - panelHeight) / 2)
        }

      val viewBoxString =
        viewBoxX.toString ++ " " ++
          viewBoxY.toString ++ " " ++ 
          Math.max(panelWidth, svgWidth).toString ++ " " ++ 
          Math.max(panelHeight, svgHeight).toString

      for {
        paper <- paperElement
      } {
        paper.attr(js.Dynamic.literal(
          "width" -> (svgWidth - (2 * panelPadding)).toString,
          "height" -> (svgHeight - (2 * panelPadding)).toString,
          "viewBox" -> viewBoxString
        ))
      }
    }

  }

}
