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

  }

}
