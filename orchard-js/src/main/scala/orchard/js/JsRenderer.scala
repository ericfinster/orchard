/**
  * JsRenderer.scala - Javascript Rendering Implementation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.js

import org.scalajs.dom
import org.scalajs.jquery._

import scalatags.JsDom.implicits._
import scalatags.JsDom.svgTags._
import scalatags.JsDom.svgAttrs._

import orchard.core.tree._

class JsRenderer(proofSheetEl : dom.Element) extends Renderer[Int] {

  val proofSheet = svg().render

  jQuery(proofSheetEl).append(proofSheet)

  def arcRadius : Double = 4.0
  def externalPadding : Double = 5.0
  def internalPadding : Double = 5.0
  def halfLeafWidth : Double = 10.0
  def halfStrokeWidth : Double = 1.0

  def getLabelBBox(a : Int) : BBox = {

    val textElement = text(a.toString).render

    jQuery(proofSheet).append(textElement)

    val bbox = new BBox {

      val width : Double = jQuery(textElement).width
      val height : Double = jQuery(textElement).height

    }

    println("Rendered text for: " ++ a.toString)
    println("Width : " ++ bbox.width.toString)
    println("Height : " ++ bbox.height.toString)

    bbox

  }

}
