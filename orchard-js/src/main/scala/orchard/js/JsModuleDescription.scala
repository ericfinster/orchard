/**
  * JsModuleDescription.scala - Client side module data description
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.js

import scala.scalajs._
import org.scalajs.dom
import org.scalajs.jquery._

import orchard.core.checker._

class JsModuleDescription(val name : String, val panelHtml : String, addr : Vector[Int]) extends ModuleDescription {

  var address : Vector[Int] = addr

  val panelDiv : JQuery = jQuery(panelHtml)
  val entriesList : JQuery = panelDiv.find(".module-entries")

  refreshCursorIndicies

  // To be sure of the order, we going to store the cursor elements in an
  // array or something.

  def insertEntryAt(entry : ModuleEntry, index : Int) : Unit = {
    entry match {
      case m : Module => {
        insertSubmoduleAt(m, index)
      }
      case _ => ()
    }
  }

  def insertSubmoduleAt(module : Module, index : Int) : Unit = {
    val desc = module.desc.asInstanceOf[JsModuleDescription]
    // Okay, we have the html.  We just need to select the right place in the
    // jQuery object and append the new module html, plus a new cursor object

    val cursorJquery = jQuery("<li class=\"cursor\"><a href=\"#\"><i class=\"fa fa-chevron-circle-right\"></i></a></li>")
    entriesList.find("> li[data-index=" ++ index.toString ++ "]").after(cursorJquery).after(desc.panelDiv)

    // Okay, I think I see what we should do.  On each cursor we should set a data
    // attribute which says it's position in the list.  It's this that we should update.
    // The reason?  Because then you can find the correct element with a jquery selection
    // and insert the appropriate html after it.

    refreshCursorIndicies
  }

  def refreshCursorIndicies : Unit = {
    entriesList.find("> .cursor").each(((i : js.Any, el : dom.Element) => {

      val index : Int = i.asInstanceOf[js.Number].toInt

      jQuery(el).attr("data-index", index.toString)

      jQuery(el).click((() => { 
        Main.NewModuleModal.targetAddress = address :+ index
        Main.NewModuleModal.show
      }) : js.Function)

    }) : js.Function2[js.Any, dom.Element, js.Any])
  }

}
