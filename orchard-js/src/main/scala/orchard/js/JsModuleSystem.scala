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

  abstract class JsNode extends Node {
    def name : String = "Unknown"
    def panelJq : JQuery
  }

  class JsModuleNode(override val name : String, val panelHtml : String) extends JsNode with ModuleNode {

    val panelJq : JQuery = jQuery(panelHtml)
    val entriesListJQ : JQuery = panelJq.find(".module-entries")
    def cursorsJQ : JQuery = entriesListJQ.find("> .cursor")

    refreshCursorIndicies

    def insertEntryAt(entry : ModuleEntry, index : Int) : Unit = {

      val cursorJQ = jQuery("<li class=\"cursor\"><a href=\"#\"><div class=\"cursor-bar\"></div></a></li>")
      entriesListJQ.find("> li[data-index=" ++ index.toString ++ "]").after(cursorJQ).after(entry.node.panelJq)
      refreshCursorIndicies

    }

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

  class JsParameterNode(override val name : String, parameterHtml : String) extends JsNode with ParameterNode {

    val panelJq : JQuery = jQuery(parameterHtml)

  }

  class JsDefinitionNode extends JsNode with DefinitionNode {

    val panelJq : JQuery = ???

  }

  class JsImportNode extends JsNode with ImportNode {

    val panelJq : JQuery = ???

  }

}
