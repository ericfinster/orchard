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
import js.Dynamic.{literal => lit}

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
  val jqModuleView : JQuery = jQuery(".module-view")

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

  jQuery("#new-worksheet-btn").click(() => {
    println("Getting a new worksheet ...")

    jQuery.getJSON("/new-worksheet", success = ((data : js.Dynamic) => {
      appendWorksheet(data.message)
    }) : js.Function1[js.Dynamic, Unit])
  })

  jQuery("#extrude-btn").click(() => {
    for {
      complex <- currentComplex
    } {
      println("Making a request for worksheet: " ++ complex.remoteId.toString)

      val descWriter = implicitly[JsonWritable[SelectionDescriptor, js.Any]]
      val desc = descWriter.write(complex.descriptor, JsJsonWriter)

      val extrudeRequest = lit(
        "worksheetId" -> complex.remoteId,
        "selectionDescriptor" -> desc
      )

      val f =
        Ajax.post(
          "/extrude-worksheet",
          js.JSON.stringify(extrudeRequest),
          0,
          Seq(("Content-type" -> "application/json")),
          false
        )

      f.onSuccess {
        case xmlReq => {
          val newJson = js.JSON.parse(xmlReq.responseText).asInstanceOf[js.Dictionary[js.Any]]

          newJson("status").asInstanceOf[js.String] match {
            case "OK" => complex.refreshFromJson(newJson("message"))
            case "KO" => Toastr.error(newJson("message").asInstanceOf[js.String])
          }

        }
      }
    }
  })

  // Put our toasts in the right place
  Toastr.options.positionClass = "toast-bottom-full-width"

  import JQueryCarousel._

  // Let's initialize the worksheet carousel
  jQuery(".worksheet-carousel").jcarousel(lit(
    "vertical" -> true
  ))

  jQuery(".worksheet-carousel-prev").jcarouselControl(lit(
    "target" -> "-=1"
  ))

  jQuery(".worksheet-carousel-next").jcarouselControl(lit(
    "target" -> "+=1"
  ))
  
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

          val newJson = js.JSON.parse(xmlReq.responseText).asInstanceOf[js.Dictionary[js.Any]]
          
          newJson("status").asInstanceOf[js.String] match {
            case "OK" => {
              val node = new JsModuleNode(moduleId, getModuleHtml(moduleId))

              for {
                root <- rootModule
                insertionPtr <- ModuleZipper(root, Nil).seek(activeModuleAddress)
                ptr <- insertionPtr.insertAt(Module(node, Vector.empty), activeCursorIndex)
              } {
                rootModule = Some(ptr.zip.asInstanceOf[Module])
                setCursorPosition(activeModuleAddress, activeCursorIndex + 1)
                Toastr.success("Created module: " ++ moduleId)
              }

            }
            case "KO" => {
              val msg = newJson("message").asInstanceOf[js.String]
              Toastr.error(msg)
            }
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

  def runErrorToast[A](err : Error[A], successMessage : String) : Unit = 
    err match {
      case Right(_) => Toastr.success(successMessage)
      case Left(msg) => Toastr.error(msg)
    }

  def getModuleHtml(moduleId : String) : String = {

    import scalatags.Text.all._

    val modHtml = 
      div(`class`:="panel panel-default module-panel")(
        div(`class`:="panel-heading")(
          h3(`class`:="panel-title")(moduleId)
        ),
        div(`class`:="panel-body")(
          ul(`class`:="module-entries")(
            li(`class`:="cursor")(a(href:="#")(div(`class`:="cursor-bar")))
          )
        )
      )

    modHtml.toString

  }

  //============================================================================================
  // AJAX REQUESTS
  //

  def requestModule(moduleId : String) : Future[dom.XMLHttpRequest] = {
    val checkerWriter = implicitly[JsonWritable[CheckerAddress, js.Any]]
    val checkerAddress = CheckerAddress(activeModuleAddress, activeCursorIndex)

    val request = lit( 
      "moduleId" -> moduleId,
      "address" -> checkerWriter.write(checkerAddress, JsJsonWriter)
    )

    Ajax.post(
      "/new-module",
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

    val node = new JsModuleNode("Prelude", getModuleHtml("Prelude"))

    node.cursorsJQ.each((i : js.Any, el : dom.Element) => {
      jQuery(el).find(".cursor-bar").addClass("active")
    })

    jqModuleView.append(node.panelJQ)
    val rm = Module(node, Vector.empty)
    rootModule = Some(rm)

  }


  def appendWorksheet(json : js.Any) : Unit = {

    val listElement = document.createElement("li")
    jQuery(".worksheet-carousel ul").append(listElement)

    val worksheet = new JsWorksheet(listElement, json, 200)
    worksheet.renderAll

    println("Successfully parsed a worksheet with id: " ++ worksheet.remoteId.toString)

    import JQueryCarousel._

    jQuery(".worksheet-carousel").jcarousel("reload", lit())

    jQuery(listElement).
      on("jcarousel:targetin", (e : JQueryEventObject, c : JCarousel) => {
        currentComplex = Some(worksheet)
      })

    jQuery(".worksheet-carousel").jcarousel("scroll", jQuery(listElement))
    currentComplex = Some(worksheet) 

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

}


object JQueryImplicits {

  implicit def jqEventHandlerAction(handler : JQueryEventObject => Unit) : js.Function1[JQueryEventObject, js.Any] =
    ((e : JQueryEventObject) => { handler(e) ; (true : js.Any) }) : js.Function1[JQueryEventObject, js.Any]

  implicit def jqEachIterator(handler : (js.Any, dom.Element) => Unit) : js.Function2[js.Any, dom.Element, js.Any] = 
    ((i : js.Any, el : dom.Element) => { handler(i, el) ; (true : js.Any) }) : js.Function2[js.Any, dom.Element, js.Any]

}
