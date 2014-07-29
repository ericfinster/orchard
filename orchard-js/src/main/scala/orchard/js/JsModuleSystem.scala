/**
  * JsModuleSystem.scala - Client side module system
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.js

import orchard.core.checker._

import scala.scalajs._
import org.scalajs.dom
import org.scalajs.jquery._

import JQueryImplicits._

trait JsModuleSystem extends ModuleSystem {

  type NodeType = JsNode
  type ModuleNodeType = JsModuleNode
  type ParameterNodeType = JsParameterNode
  type DefinitionNodeType = JsDefinitionNode
  type ImportNodeType = JsImportNode

  class JsNode extends Node {
    def name : String = "Unknown"
  }

  class JsModuleNode(override val name : String, val panelHtml : String) extends JsNode with ModuleNode {

    val panelJQ : JQuery = jQuery(panelHtml)
    val entriesListJQ : JQuery = panelJQ.find(".module-entries")

    refreshCursorIndicies

    def insertEntryAt(entry : ModuleEntry, index : Int) : Unit = {
      entry match {
        case m : Module => {
          insertSubmoduleAt(m, index)
        }
        case _ => ()
      }
    }

    def insertSubmoduleAt(module : Module, index : Int) : Unit = {
      val node : JsModuleNode = module.node

      // Okay, we have the html.  We just need to select the right place in the
      // jQuery object and append the new module html, plus a new cursor object

      val cursorJQ = jQuery("<li class=\"cursor\"><a href=\"#\"><div class=\"cursor-bar\"></div></a></li>")
      entriesListJQ.find("> li[data-index=" ++ index.toString ++ "]").after(cursorJQ).after(node.panelJQ)

      // Okay, I think I see what we should do.  On each cursor we should set a data
      // attribute which says it's position in the list.  It's this that we should update.
      // The reason?  Because then you can find the correct element with a jquery selection
      // and insert the appropriate html after it.

      refreshCursorIndicies
    }

    def cursorsJQ : JQuery = entriesListJQ.find("> .cursor")

    def refreshCursorIndicies : Unit = {
      cursorsJQ.each((i : js.Any, el : dom.Element) => {
        val index : Int = i.asInstanceOf[js.Number].toInt

        jQuery(el).attr("data-index", index.toString)

        jQuery(el).click(() => {
          Main.setCursorPosition(address, index)
        })
      })
    }


  }

  class JsParameterNode extends JsNode with ParameterNode
  class JsDefinitionNode extends JsNode with DefinitionNode
  class JsImportNode extends JsNode with ImportNode

}
