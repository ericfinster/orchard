/**
  * Template.scala - Testing template layout
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.js

import scala.scalajs._

import org.scalajs.dom
import org.scalajs.jquery._

import dom.document
import dom.window

import js.annotation.JSExport
import js.Dynamic.{literal => lit}

import plugins.PerfectScrollbar._

@JSExport
object Template {

  val windowJQ = jQuery(window)
  val moduleViewJQ = jQuery(".module-view")
  val worksheetViewJQ = jQuery(".worksheet-view")

  @JSExport
  def initialize() : Unit = {

    doLayout

    jQuery(window).resize(() => {
      doLayout
    })

    moduleViewJQ.perfectScrollbar()
    
  }

  def doLayout() : Unit = {

    val windowHeight = windowJQ.height

    moduleViewJQ.height(200);
    worksheetViewJQ.height(windowHeight - 300);

    moduleViewJQ.perfectScrollbar("update");

  }

}
