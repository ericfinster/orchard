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

import scalatags.JsDom.all._

import JQueryImplicits._

trait JsModuleSystem extends ModuleSystem {

  type NodeType = JsNode
  type ContainerNodeType = JsContainerNode
  type ExpressionNodeType = JsExpressionNode
  type ModuleNodeType = JsModuleNode
  type ParameterNodeType = JsParameterNode
  type DefinitionNodeType = JsDefinitionNode
  type ImportNodeType = JsImportNode

  abstract class JsNode extends Node {
    def panelJq : JQuery
  }

  object JsNode {

    import Node._

    implicit object JsNodeGenerator extends SummaryNodeGenerator {
      def createModule(name : String) : ModuleNodeType = new JsModuleNode(name)
      def createImport(name : String, moduleName : String, isOpen : Boolean) : ImportNodeType = 
        new JsImportNode(name, moduleName, isOpen)
      def createParameter(name : String) : ParameterNodeType = new JsParameterNode(name)
      def createDefinition(name : String) : DefinitionNodeType = new JsDefinitionNode(name)
    }

  }

  abstract class JsContainerNode extends JsNode with ContainerNode
  abstract class JsExpressionNode extends JsNode with ExpressionNode

  class JsModuleNode(val name : String) extends JsContainerNode with ModuleNode {

    val modElement =
      div(`class`:="panel panel-default module-panel")(
        div(`class`:="panel-heading")(
          h3(`class`:="panel-title")(name),
          div(`class`:="panel-tools")(
            a(`class`:="btn btn-link btn-sm panel-collapse", `href`:="#")(i(`class`:="fa fa-chevron-down"))
          )
        ),
        div(`class`:="panel-body")(
          ul(`class`:="module-entries")(
            li(`class`:="cursor")(a(href:="#")(div(`class`:="cursor-bar")))
          )
        )
      ).render

    val panelJq : JQuery = jQuery(modElement)
    val panelHeadingJq : JQuery = panelJq.children(".panel-heading")
    val panelBodyJq : JQuery = panelJq.children(".panel-body")
    val entriesListJQ : JQuery = panelBodyJq.children(".module-entries")

    def cursorsJQ : JQuery = entriesListJQ.find("> .cursor")


    panelHeadingJq.find(".panel-collapse").click(() => {
      if (panelBodyJq.hasClass("panel-collapsed")) {
        panelBodyJq.slideDown(200, () => {
          panelBodyJq.removeClass("panel-collapsed")
        })
      } else {
        panelBodyJq.slideUp(200, () => {
          panelBodyJq.addClass("panel-collapsed")
        })
      }
    })

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

  class JsImportNode(val name : String, val moduleName : String, val isOpen : Boolean) extends JsContainerNode with ImportNode {

    val importElement = 
      div(`class`:="panel panel-default import-panel")(
        div(`class`:="panel-heading")(
          h3(`class`:="panel-title")(name),
          div(`class`:="panel-tools")(
            a(`class`:="btn btn-link btn-sm panel-collapse", `href`:="#")(i(`class`:="fa fa-chevron-down"))
          )
        ),
        div(`class`:="panel-body panel-collapsed")()
      ).render

    val panelJq : JQuery = jQuery(importElement)
    val panelHeadingJq : JQuery = panelJq.children(".panel-heading")
    val panelBodyJq : JQuery = panelJq.children(".panel-body")

    panelHeadingJq.find(".panel-collapse").click(() => {
      if (panelBodyJq.hasClass("panel-collapsed")) {
        panelBodyJq.slideDown(200, () => {
          panelBodyJq.removeClass("panel-collapsed")
        })
      } else {
        panelBodyJq.slideUp(200, () => {
          panelBodyJq.addClass("panel-collapsed")
        })
      }
    })

  }


  class JsParameterNode(val name : String) extends JsExpressionNode with ParameterNode {

    val parameterElement = 
      div(`class`:="panel panel-default parameter-panel")(
        div(`class`:="panel-heading")(
          h3(`class`:="panel-title")(name),
          div(`class`:="panel-tools")(
            a(`class`:="btn btn-link btn-sm panel-collapse", `href`:="#")(i(`class`:="fa fa-chevron-down"))
          )
        ),
        div(`class`:="panel-body panel-collapsed")()
      ).render

    val panelJq : JQuery = jQuery(parameterElement)
    val panelHeadingJq : JQuery = panelJq.children(".panel-heading")
    val panelBodyJq : JQuery = panelJq.children(".panel-body")

    panelHeadingJq.find(".panel-collapse").click(() => {
      if (panelBodyJq.hasClass("panel-collapsed")) {
        panelBodyJq.slideDown(200, () => {
          panelBodyJq.removeClass("panel-collapsed")
        })
      } else {
        panelBodyJq.slideUp(200, () => {
          panelBodyJq.addClass("panel-collapsed")
        })
      }
    })

  }

  class JsDefinitionNode(val name : String) extends JsExpressionNode with DefinitionNode {

    val definitionElement = 
      div(`class`:="panel panel-default definition-panel")(
        div(`class`:="panel-heading")(
          h3(`class`:="panel-title")(name),
          div(`class`:="panel-tools")(
            a(`class`:="btn btn-link btn-sm panel-collapse", `href`:="#")(i(`class`:="fa fa-chevron-down"))
          )
        ),
        div(`class`:="panel-body panel-collapsed")()
      ).render

    val panelJq : JQuery = jQuery(definitionElement)
    val panelHeadingJq : JQuery = panelJq.children(".panel-heading")
    val panelBodyJq : JQuery = panelJq.children(".panel-body")

    panelHeadingJq.find(".panel-collapse").click(() => {
      if (panelBodyJq.hasClass("panel-collapsed")) {
        panelBodyJq.slideDown(200, () => {
          panelBodyJq.removeClass("panel-collapsed")
        })
      } else {
        panelBodyJq.slideUp(200, () => {
          panelBodyJq.addClass("panel-collapsed")
        })
      }
    })

  }

}
