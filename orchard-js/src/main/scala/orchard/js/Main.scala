/**
  * Main.scala - A Main object for Orchard client side
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.js

import scala.scalajs._
import org.scalajs.dom
import dom.document

import org.scalajs.jquery.jQuery

import js.annotation.JSExport

import orchard.core.util._

object Main extends js.JSApp {

  def main(): Unit = {
    println("Starting Orchard ...")

    jQuery.getJSON("/complex", success = ((data : js.Object) => {
      renderComplex(data)
    }) : js.Function1[js.Object, Unit])

  }

  @JSExport
  def logMessage(): Unit = {
    println("This is the log message.")
  }

  @JSExport
  def renderComplex(json : js.Any) : Unit = {
    println("Received some Json.")

    val reader = JsJsonReader

    val d = document.createElement("div")
    jQuery(d).appendTo("#workspace")

    val complex = new JsComplex[String](d, json, 300, 300, 3)
    
    complex.renderAll

    import JQueryCarousel._

    // Now I need to run jcarousel and start him ...
    jQuery(".jcarousel").jcarousel
    jQuery(".jcarousel-prev").jcarouselControl(js.Dynamic.literal(("target" -> "-=1")))
    jQuery(".jcarousel-next").jcarouselControl(js.Dynamic.literal(("target" -> "+=1")))

    // Okay, I think rather the idea should be that you create a gallery with a given
    // div, and the gallery (on creation) puts all it's content inside. and starts the
    // carousel.  Then the call to renderall will infact set all of the sizes and render
    // everything.

  }

}
