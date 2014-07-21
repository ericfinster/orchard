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
    complex.generatePanels
    complex.renderAll
  }

  // @JSExport
  // def createSVGElement() : Unit = {

  //   val svgNS = "http://www.w3.org/2000/svg"

  //   val d = document.createElementNS(svgNS, "svg")

  //   d.setAttributeNS(svgNS, "id", "mysvg")
  //   d.setAttributeNS(svgNS, "width", "100")
  //   d.setAttributeNS(svgNS, "height", "100")
  //   d.setAttributeNS(svgNS, "version", "1.1")

  //   val g = document.getElementById("gallery")
  //   g.appendChild(d)

  // }

  // @JSExport
  // def createSnapSurface() : Unit = {

  //   println("Attempting to create snap surface ...")

  //   val paper = Snap("#mysvg")

  //   println("Done.")

  // }

}

