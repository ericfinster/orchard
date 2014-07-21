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
  }

  @JSExport
  def logMessage(): Unit = {
    println("This is the log message.")
  }

  @JSExport
  def renderComplex(json : js.Any) : Unit = {
    println("Received some Json.")

    val reader = JsJsonReader
    val stringReader = implicitly[JsonReadable[String, js.Any]]

    val complex = new JsComplex[String]
    
    val newTopCell = complex.fromJson(json, JsJsonReader, stringReader)

    println("Parsed complex.")

    complex.topCell = newTopCell

    jQuery("#workspace").append(complex.getContent)
    complex.renderAll

    import JQueryCarousel._

    // Now I need to run jcarousel and start him ...
    jQuery("#gallery").jcarousel

  }

}
