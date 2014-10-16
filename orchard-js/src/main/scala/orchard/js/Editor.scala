/**
  * Editor.scala - The Main Application Class
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.js

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue
import scala.concurrent.Future

import scala.scalajs._
import js.Dynamic.{literal => lit}
import js.annotation.JSExport

import org.scalajs.dom
import org.scalajs.jquery._

import dom.document
import dom.extensions._

import scalatags.JsDom.implicits._
import scalatags.JsDom.svgTags._
import scalatags.JsDom.svgAttrs._

import orchard.js.plugins._
import orchard.core.util._
import ErrorM._

trait Editor
    extends EditorUI
    with EditorEvents
    with EditorAjax
    with EditorDialogs
    with EditorImplicits {

  var focus : Vector[Int] = Vector.empty

}

object TheEditor extends js.JSApp with Editor {

  //============================================================================================
  // MAIN ENTRY POINT
  //

  def main(): Unit = {

    println("Starting Orchard Editor ...")

    // Do a first layout
    doLayout

    val renderer : JsRenderer = new JsRenderer(dom.document.getElementById("test-body"))

    import orchard.core.tree.NestingExamples._

    for {
      result <- renderer.renderComplex(fred)
    } {
      println("Rendering complete!")
    }

  }

}

