/**
  * Main.scala - A Main object for Orchard client side
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

import org.scalajs.jquery._

import js.annotation.JSExport

import scala.concurrent.Future

import orchard.core.util._
import orchard.core.complex._
import orchard.core.checker._

import orchard.js.plugins._

import ErrorM._

object Main extends js.JSApp with JsModuleSystem {

  //============================================================================================
  // UI INITIALIZATION
  //

  import JQueryImplicits._

  val jqMain : JQuery = jQuery(".main")

  // val jqControlPanel : JQuery = jQuery(".control-panel")
  // val jqControlPaneSlider : JQuery = jQuery(".control-pane-slider")

  // jqControlPaneSlider.mousedown((e : JQueryEventObject) => {

  //   val mouseOriginY : Double = e.pageY
  //   val footerHeight : Double = jqFooter.height()

  //   val mousemoveHandler : js.Function1[JQueryEventObject, js.Any] = 
  //     ((me : JQueryEventObject) => {
  //       me.preventDefault
  //       jqFooter.css("height", footerHeight + (mouseOriginY - me.pageY))
  //     })

  //   jQuery(document).on("mousemove", mousemoveHandler)
  //   jQuery(document).one("mouseup", (e : JQueryEventObject) => {
  //     jQuery(document).off("mousemove", mousemoveHandler)
  //   })

  // })

  jQuery("#new-module-btn").click(() => {
    println("New module button clicked ...") 
    NewModuleModal.show
  })

  jQuery("#extrude-btn").click(() => {
    for {
      complex <- currentComplex
    } {
      val descWriter = implicitly[JsonWritable[SelectionDescriptor, js.Any]]
      val desc = descWriter.write(complex.descriptor, JsJsonWriter).asInstanceOf[js.Object]

      val f =
        Ajax.post(
          "http://localhost:9000/extrude",
          js.JSON.stringify(desc),
          0,
          Seq(("Content-type" -> "application/json")),
          false
        )

      f.onSuccess {
        case xmlReq => {
          println("Success!")
          val newJson = js.JSON.parse(xmlReq.responseText).asInstanceOf[js.Dictionary[js.Any]]
          complex.refreshFromJson(newJson("message"))
        }
      }
      f.onFailure { case _ => println("Failure.") }
    }
  })

  //============================================================================================
  // DIALOG DEFINITIONS
  //

  object NewModuleModal extends BootstrapModal("orchard-new-module-modal") {

    override def onShow = { 
      println("Active module address: " ++ activeModuleAddress.toString)
      println("Active cursor index: " ++ activeCursorIndex.toString)
    }

    override def onHide = {
      // Really we need to check the response status ...
      val moduleId: String = modalJQuery.find("#module-name").value.asInstanceOf[js.String]

      requestModule(moduleId) onSuccess {
        case xmlReq => {
          val node = new JsModuleNode(moduleId, xmlReq.responseText)

          // Now what? We have to append this guy to the correct spot in the module which
          // we are pointing at.

          for {
            root <- rootModule
            insertionPtr <- ModuleZipper(root, Nil).seek(activeModuleAddress)
            ptr <- insertionPtr.insertAt(Module(node, Vector.empty), activeCursorIndex)
          } {
            rootModule = Some(ptr.zip.asInstanceOf[Module])
            setCursorPosition(activeModuleAddress, activeCursorIndex + 1)
          }
        }
      }
    }

  }

  //============================================================================================
  // EDITOR VARIABLES
  //

  var rootModule : Error[Module] = fail("No active module.")

  var activeModuleAddress : Vector[Int] = Vector.empty
  var activeCursorIndex : Int = 0

  def seekModule(addr : Vector[Int]) : Error[Module] = 
    for {
      root <- rootModule
      modulePtr <- ModuleZipper(root, Nil).seek(addr)
      module <- modulePtr.focusedModule
    } yield module

  def cursorJQ(addr : Vector[Int], index : Int) : Error[JQuery] = 
    for {
      module <- seekModule(addr)
    } yield {
      jQuery(module.node.cursorsJQ.get(index))
    }

  def activeModule : Error[Module] = 
    seekModule(activeModuleAddress)

  def activeCursorJQ : Error[JQuery] = 
    cursorJQ(activeModuleAddress, activeCursorIndex)

  def setCursorPosition(moduleAddress : Vector[Int], cursorIndex : Int) : Unit = 
    for {
      acJQ <- activeCursorJQ
      cJQ <- cursorJQ(moduleAddress, cursorIndex)
    } {
      acJQ.find(".cursor-bar").removeClass("active")
      cJQ.find(".cursor-bar").addClass("active")
      activeModuleAddress = moduleAddress
      activeCursorIndex = cursorIndex
    }

  //============================================================================================
  // AJAX REQUESTS
  //

  def requestModule(moduleId : String) : Future[dom.XMLHttpRequest] = {
    val request = js.Dynamic.literal( "moduleId" -> moduleId )

    Ajax.post(
      "/newmodule",
      js.JSON.stringify(request),
      0,
      Seq(("Content-type" -> "application/json")),
      false
    )
  }

  //============================================================================================
  // MAIN ENTRY POINT
  //

  def main(): Unit = {
    println("Starting Orchard ...")

    requestModule("Prelude") onSuccess { 
      case xmlReq => {
        val node = new JsModuleNode("Prelude", xmlReq.responseText)

        node.cursorsJQ.each((i : js.Any, el : dom.Element) => {
          jQuery(el).find(".cursor-bar").addClass("active")
        })

        jqMain.append(node.panelJQ)
        val rm = Module(node, Vector.empty)
        rootModule = Some(rm)
      }
    }

    jQuery.getJSON("/worksheet", success = ((data : js.Object) => {
      renderComplex(data)
    }) : js.Function1[js.Object, Unit])

  }

  //============================================================================================
  // OLD TESTS AND EXAMPLES
  //

  // jQuery(".panel").each(((i : js.Any, el : dom.Element) => {

  // jQuery(el).mouseover(((e: JQueryEventObject) => {
  //   jQuery(el).css("box-shadow", "0 1px 5px #2f2f2f")
  // }) : js.Function1[JQueryEventObject, js.Any])

  // jQuery(el).mouseout(((e: JQueryEventObject) => {
  //   jQuery(el).css("box-shadow", "0 1px 5px #c3c3c3")
  // }) : js.Function1[JQueryEventObject, js.Any])

  // jQuery(el).hover(
  //   ((e: JQueryEventObject) => {
  //     println("hoverIn")
  //     jQuery(el).css("box-shadow", "0 1px 5px #2f2f2f")
  //   }) : js.Function1[JQueryEventObject, js.Any],
  //   ((e: JQueryEventObject) => {
  //     println("hoverOut")
  //     jQuery(el).css("box-shadow", "0 1px 5px #c3c3c3")
  //   }) : js.Function1[JQueryEventObject, js.Any]
  // )

  // }) : js.Function2[js.Any, dom.Element, js.Any])

  var currentComplex : Option[JsWorksheet] = None

  def renderComplex(json : js.Any) : Unit = {

    val controlPanelJQ = jQuery(".control-panel-gallery")
    val controlPanelDiv = controlPanelJQ.get(0)

    controlPanelJQ.empty()

    val complex = new JsWorksheet(controlPanelDiv.asInstanceOf[dom.Element], json, 100, 100, 3)

    complex.renderAll
    currentComplex = Some(complex)

  }

}


object JQueryImplicits {

  implicit def jqEventHandlerAction(handler : JQueryEventObject => Unit) : js.Function1[JQueryEventObject, js.Any] =
    ((e : JQueryEventObject) => { handler(e) ; (true : js.Any) }) : js.Function1[JQueryEventObject, js.Any]

  implicit def jqEachIterator(handler : (js.Any, dom.Element) => Unit) : js.Function2[js.Any, dom.Element, js.Any] = 
    ((i : js.Any, el : dom.Element) => { handler(i, el) ; (true : js.Any) }) : js.Function2[js.Any, dom.Element, js.Any]

}
