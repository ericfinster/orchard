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
  val jqModuleWrapper : JQuery = jQuery(".module-wrapper")

  jQuery("#new-module-btn").click(() => {
    NewModuleModal.show
  })

  jQuery("#new-parameter-btn").click(() => {
    NewParameterModal.show
  })

  jQuery("#new-worksheet-btn").click(() => {
    jQuery.getJSON("/new-worksheet", success = ((data : js.Dynamic) => {
      appendWorksheet(data.message)
    }) : js.Function1[js.Dynamic, Unit])
  })

  jQuery("#extrude-btn").click(() => {
    for {
      complex <- currentComplex
    } {

      val extrudeRequest = lit(
        "worksheetId" -> complex.remoteId,
        "selectionDescriptor" -> JsJsonWriter.write(complex.descriptor)
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
      // println("Active module address: " ++ activeModuleAddress.toString)
      // println("Active cursor index: " ++ activeCursorOffset.toString)
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
                ptr <- insertionPtr.insertAt(Module(node, Vector.empty), activeCursorOffset)
              } {
                rootModule = Some(ptr.zip.asInstanceOf[Module])
                setCursorPosition(activeModuleAddress, activeCursorOffset + 1)
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

  object NewParameterModal extends BootstrapModal("orchard-new-parameter-modal") {

    override def onShow = {
      println("Showing new parameter dialog")
    }

    override def onHide = {
      println("Hiding new parameter dialog")
    }

  }

  //============================================================================================
  // EDITOR VARIABLES
  //

  var rootModule : Error[Module] = fail("No active module.")
  var hasEnvironment : Boolean = false
  var currentComplex : Option[JsWorksheet] = None
  var activeCheckerAddress : CheckerAddress = 
    CheckerAddress(Vector.empty, 0)

  def activeModuleAddress = activeCheckerAddress.moduleAddress
  def activeCursorOffset = activeCheckerAddress.cursorOffset

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
    cursorJQ(activeModuleAddress, activeCursorOffset)

  def setCursorPosition(moduleAddress : Vector[Int], cursorIndex : Int) : Unit = 
    for {
      acJQ <- activeCursorJQ
      cJQ <- cursorJQ(moduleAddress, cursorIndex)
    } {
      acJQ.find(".cursor-bar").removeClass("active")
      cJQ.find(".cursor-bar").addClass("active")
      activeCheckerAddress = CheckerAddress(moduleAddress, cursorIndex)
      updateEnvironment
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

    val request = lit( 
      "moduleId" -> moduleId,
      "address" -> JsJsonWriter.write(activeCheckerAddress)
    )

    Ajax.post(
      "/new-module",
      js.JSON.stringify(request),
      0,
      Seq(("Content-type" -> "application/json")),
      false
    )
  }

  def requestEnvironment : Future[dom.XMLHttpRequest] = {

    val request = lit(
      "address" -> JsJsonWriter.write(activeCheckerAddress)
    )

    Ajax.post(
      "/environment",
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

    jqModuleWrapper.append(node.panelJQ)
    val rm = Module(node, Vector.empty)
    rootModule = Some(rm)

    setCursorPosition(Vector.empty, 0)

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

  def updateEnvironment : Unit = {

    import BootstrapTreeview._

    if (hasEnvironment) {
      jQuery("#env-tree").treeview("remove")
      hasEnvironment = false
    }

    requestEnvironment onSuccess {
      case xmlReq => {

        val jsonResponse = js.JSON.parse(xmlReq.responseText).asInstanceOf[js.Dictionary[js.Any]]
        
        jsonResponse("status").asInstanceOf[js.String] match {
          case "OK" => {

            val envRoseTree = jsonResponse("message").as[RoseTree[String, String]]

            def roseTreeToJs(t : RoseTree[String, String]) : js.Any = 
              t match {
                case Rose(s) => lit(text = s)
                case Branch(s, brs) => lit(
                  text = s,
                  nodes = js.Array(brs map (roseTreeToJs(_)) : _*)
                )
              }

            jQuery("#env-tree").treeview(lit(
              data = js.Array(roseTreeToJs(envRoseTree))
            ))

            hasEnvironment = true
          }
          case "KO" => {
            val msg = jsonResponse("message").asInstanceOf[js.String]
            Toastr.error(msg)
          }
        }
      }
    }
  }

  implicit class JsAnyOps(x : js.Any) {

    def as[A](implicit aReader : JsonReadable[A, js.Any]) : A = 
      aReader.read(x, JsJsonReader)

  }

}


object JQueryImplicits {

  implicit def jqEventHandlerAction(handler : JQueryEventObject => Unit) : js.Function1[JQueryEventObject, js.Any] =
    ((e : JQueryEventObject) => { handler(e) ; (true : js.Any) }) : js.Function1[JQueryEventObject, js.Any]

  implicit def jqEachIterator(handler : (js.Any, dom.Element) => Unit) : js.Function2[js.Any, dom.Element, js.Any] = 
    ((i : js.Any, el : dom.Element) => { handler(i, el) ; (true : js.Any) }) : js.Function2[js.Any, dom.Element, js.Any]

}
