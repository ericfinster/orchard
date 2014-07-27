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

import ErrorM._

object Main extends js.JSApp {

  //============================================================================================
  // UI INITIALIZATION
  //

  import JQueryImplicits._

  val jqOrchardMain : JQuery = jQuery("#orchard-main")
  val jqOrchardControl : JQuery = jQuery(".orchard-control-content")
  val jqOrchardSplitSlider : JQuery = jQuery(".orchard-split-pane-slider")

  jqOrchardSplitSlider.mousedown((e : JQueryEventObject) => {

    val mouseOriginY : Double = e.pageY
    val controlHeight : Double = jqOrchardControl.height()

    val mousemoveHandler : js.Function1[JQueryEventObject, js.Any] = 
      ((me : JQueryEventObject) => {
        me.preventDefault
        jqOrchardControl.css("height", controlHeight + (mouseOriginY - me.pageY))
      })

    jQuery(document).on("mousemove", mousemoveHandler)
    jQuery(document).one("mouseup", (e : JQueryEventObject) => {
      jQuery(document).off("mousemove", mousemoveHandler)
    })

  })

  val windowWidth = jQuery(dom.window).width()
  val windowHeight = jQuery(dom.window).height()

  jQuery(".main-content").css("min-height", windowHeight)

  //============================================================================================
  // DIALOG DEFINITIONS
  //

  object NewModuleModal extends BootstrapModal("newModuleModal") {

    var targetAddress : Vector[Int] = Vector.empty

    override def onShow = { 
      println("Running new module with target " ++ targetAddress.toString)
    }

    override def onHide = {
      // Really we need to check the response status ...
      val moduleId: String = modalJQuery.find("#module-name").value.asInstanceOf[js.String]

      requestModule(moduleId) onSuccess {
        case xmlReq => {
          val desc = new JsModuleDescription(moduleId, xmlReq.responseText, Vector.empty)

          // Now what? We have to append this guy to the correct spot in the module which
          // we are pointing at.

          for {
            root <- rootModule
            insertionPtr <- ModuleZipper(root, Nil).seek(targetAddress.init)
            ptr <- insertionPtr.insertAt(Module(desc, Vector.empty), targetAddress.last)
          } {
            println("Completed insertion")
            rootModule = Some(ptr.zip.asInstanceOf[Module])
          }
        }
      }
    }

  }

  //============================================================================================
  // EDITOR VARIABLES
  //

  var rootModule : Option[Module] = None
  var modulePointer : Option[ModuleZipper] = None

  //============================================================================================
  // EDITOR ROUTINES
  //

  def main(): Unit = {
    println("Starting Orchard ...")

    requestModule("Prelude") onSuccess { 
      case xmlReq => {
        val desc = new JsModuleDescription("Prelude", xmlReq.responseText, Vector.empty)
        jqOrchardMain.append(desc.panelDiv)
        val rm = Module(desc, Vector.empty)
        rootModule = Some(rm)
      }
    }

  }

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

  def initializeExtrudeButton : Unit = {

    jQuery("#extrude").click((() => {
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
    }) : js.Function)

  }

  def renderComplex(json : js.Any) : Unit = {

    jQuery.getJSON("/worksheet", success = ((data : js.Object) => {
      renderComplex(data)
    }) : js.Function1[js.Object, Unit])


    val d = document.getElementById("workspace")
    jQuery(d).empty()
    val complex = new JsWorksheet(d, json, 200, 200, 3)
    complex.renderAll
    currentComplex = Some(complex)

  }

}


object JQueryImplicits {

  implicit def jqEventHandlerAction(handler : JQueryEventObject => Unit) : js.Function1[JQueryEventObject, js.Any] =
    ((e : JQueryEventObject) => { handler(e) ; (true : js.Any) }) : js.Function1[JQueryEventObject, js.Any]

}
