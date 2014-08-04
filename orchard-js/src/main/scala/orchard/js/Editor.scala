/**
  * Editor.scala - The Main Application Class
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.js

import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import scala.scalajs._
import org.scalajs.dom
import dom.document
import dom.extensions._
import js.Dynamic.{literal => lit}

import org.scalajs.jquery._

import js.annotation.JSExport

import scala.concurrent.Future

import orchard.core.cell._
import orchard.core.util._
import orchard.core.complex._
import orchard.core.checker._

import orchard.js.plugins._

import ErrorM._

trait Editor
    extends EditorUI
    with EditorEvents
    with EditorAjax
    with EditorDialogs
    with EditorImplicits

object TheEditor extends js.JSApp with Editor {

  //============================================================================================
  // MAIN ENTRY POINT
  //

  def main(): Unit = {
    println("Starting Orchard Editor ...")

    // Do a first layout
    doLayout

  }

}

