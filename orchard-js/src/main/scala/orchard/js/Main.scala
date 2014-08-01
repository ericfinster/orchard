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

import orchard.core.cell._
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

  jQuery("#new-module-btn").click(() => { onNewModule })
  jQuery("#new-parameter-btn").click(() => { onNewParameter })
  jQuery("#new-defn-btn").click(() => { onNewDefinition })
  jQuery("#new-worksheet-btn").click(() => { onNewWorksheet })
  jQuery("#extrude-btn").click(() => { onExtrude })
  jQuery("#drop-btn").click(() => { onDrop })

  val keyHandler : js.Function1[JQueryEventObject, js.Boolean] = 
    (e : JQueryEventObject) => {
      val keyCode : Int = e.which

      e.which match {
        case 69 => onExtrude         // e
        case 77 => onNewModule       // m
        case 80 => onNewParameter    // p
        case 70 => onNewDefinition   // f
        case 87 => onNewWorksheet    // w
        case 68 => onDrop            // d
        case _ => ()
      }

      //println("Keycode: " ++ keyCode.toString)

      true
    }

  def installKeyHandler : Unit = {
    jQuery(dom.document).on("keydown", keyHandler)
  }

  def removeKeyHandler : Unit = {
    jQuery(dom.document).off("keydown", keyHandler)
  }

  jQuery(dom.window).resize(() => { doLayout })

  doLayout
  installKeyHandler

  def doLayout : Unit = {

    val windowHeight = jQuery(dom.window).height()
    val headerNavHeight = jQuery(".header-nav").outerHeight()
    val footerNavHeight = jQuery(".footer-nav").outerHeight()
    val worksheetViewHeight = jQuery(".worksheet-view").outerHeight()

    val moduleViewJq = jQuery(".module-view").height(
      windowHeight - headerNavHeight - footerNavHeight - worksheetViewHeight - 20 - 20
    )

  }

  // Put our toasts in the right place
  Toastr.options.positionClass = "toast-bottom-full-width"

  //============================================================================================
  // DIALOG DEFINITIONS
  //

  object NewModuleModal extends BootstrapModal("orchard-new-module-modal") {

    val inputJq = modalJq.find("#module-name")

    modalJq.find("#module-form").on("submit", () => {
      // Really we need to check the response status ...
      val moduleId : String = modalJq.find("#module-name").value.asInstanceOf[js.String]
      val modAddr = activeCheckerAddress.moduleAddress
      val curOff = activeCheckerAddress.cursorOffset

      requestModule(moduleId) onSuccess {
        case returnedId : String => {

          val node = new JsModuleNode(returnedId)

          for {
            rootZ <- rootZipper
            insertionPtr <- rootZ.seek(modAddr)
            ptr <- insertionPtr.insertAt(Module(node, Vector.empty), curOff)
          } {
            rootModule = Some(ptr.zip.asInstanceOf[Module])
            setCursorPosition(modAddr, curOff + 1)
            Toastr.success("Created module: " ++ returnedId)
          }
        }
      }

      hide
      false
    })

    override def onShow = {
      removeKeyHandler
    }

    override def onShown = {
      inputJq.focus()
    }


    override def onHide = {
      installKeyHandler
      inputJq.value("")
    }
  }

  object NewParameterModal extends BootstrapModal("orchard-new-parameter-modal") {

    val inputJq = modalJq.find("#parameter-name")

    modalJq.find("#parameter-form").on("submit", () => {

      for {
        worksheet <- activeWorksheet
        targetCell <- worksheet.selectionBase
      } {

        val parameterIdent: String = modalJq.find("#parameter-name").value.asInstanceOf[js.String]
        val modAddr = activeCheckerAddress.moduleAddress
        val curOff = activeCheckerAddress.cursorOffset

        requestParameter(worksheet.remoteId, targetCell.address, parameterIdent, false) onSuccess {
          case parameterName => {

            val node = new JsParameterNode(parameterName)

            for {
              rootZ <- rootZipper
              insertionPtr <- rootZ.seek(modAddr)
              ptr <- insertionPtr.insertAt(Parameter(node), curOff)
            } {
              rootModule = Some(ptr.zip.asInstanceOf[Module])
              setCursorPosition(modAddr, curOff + 1)
              refreshWorksheet(worksheet)
              Toastr.success("Created parameter: " ++ parameterName)
            }
          }
        }
      }

      hide
      false

    })

    override def onShow = {
      removeKeyHandler
    }

    override def onShown = {
      inputJq.focus()
    }

    override def onHide = {
      installKeyHandler
      inputJq.value("")
    }
  }

  object NewDefinitionModal extends BootstrapModal("orchard-new-definition-modal") {

    val inputJq = modalJq.find("#definition-name")

    modalJq.find("#definition-form").on("submit", () => {

      for {
        worksheet <- activeWorksheet
        targetCell <- worksheet.selectionBase
      } {

        val definitionIdent: String = modalJq.find("#definition-name").value.asInstanceOf[js.String]
        val modAddr = activeCheckerAddress.moduleAddress
        val curOff = activeCheckerAddress.cursorOffset

        requestDefinition(worksheet.remoteId, targetCell.address, definitionIdent) onSuccess {
          case definitionName => {

            val node = new JsDefinitionNode(definitionName)

            for {
              rootZ <- rootZipper
              insertionPtr <- rootZ.seek(modAddr)
              ptr <- insertionPtr.insertAt(Definition(node), curOff)
            } {
              rootModule = Some(ptr.zip.asInstanceOf[Module])
              setCursorPosition(modAddr, curOff + 1)
              refreshWorksheet(worksheet)
              Toastr.success("Created definition: " ++ definitionName)
            }
          }
        }
      }

      hide
      false

    })

    override def onShow = {
      removeKeyHandler
    }

    override def onShown = {
      inputJq.focus()
    }

    override def onHide = {
      installKeyHandler
      inputJq.value("")
    }
  }

  //============================================================================================
  // EDITOR VARIABLES
  //

  var rootModule : Error[Module] = fail("No active module.")

  def rootZipper : Error[ModuleZipper] = 
    for {
      rootM <- rootModule
    } yield ModuleZipper(rootM, Nil)

  var hasEnvironment : Boolean = false
  var activeWorksheet : Option[JsWorksheet] = None
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

  def updateEnvironment : Unit = {

    import BootstrapTreeview._

    if (hasEnvironment) {
      jQuery("#env-tree").treeview("remove")
      hasEnvironment = false
    }

    requestEnvironment onSuccess {
      case Branch("root", branches) => {

        def processBranches(brs : Vector[RoseTree[String, String]]) : js.Array[js.Any] = {
          val branchArray = new js.Array[js.Any](brs.length)

          for {
            (b, i) <- brs.zipWithIndex
          } {
            branchArray(i) = roseTreeToJs(b)
          }

          branchArray
        }

        def roseTreeToJs(t : RoseTree[String, String]) : js.Any =
          t match {
            case Rose(s) => lit(text = s, icon = "none")
            case Branch(s, brs) => {
              lit(
                text = s,
                nodes = processBranches(brs),
                icon = "none"
              )
            }
          }

        jQuery("#env-tree").treeview(lit(
          data = processBranches(branches)
        ))

        hasEnvironment = true
      }
    }
  }

  //============================================================================================
  // EVENTS
  //

  def onNewWorksheet : Unit = {

    val listElement = document.createElement("li")
    jQuery(".worksheet-carousel ul").append(listElement)

    requestNewWorksheet(listElement, 200) onSuccess {
      case worksheet => {

        worksheet.renderAll

        import JCarousel._

        jQuery(".worksheet-carousel").jcarousel("reload")
        jQuery(".worksheet-carousel").jcarousel("scroll", jQuery(listElement))

        jQuery(listElement).
          on("jcarousel:targetin", "li", (e : JQueryEventObject, c : JCarousel) => {
            activeWorksheet = Some(worksheet)
          })

        activeWorksheet = Some(worksheet)

      }
    }

  }

  def onExtrude : Unit = {
    for {
      worksheet <- activeWorksheet
    } {
      requestExtrusion(worksheet)
    }
  }

  def onDrop : Unit = {
    for {
      worksheet <- activeWorksheet
    } {
      requestDrop(worksheet)
    }
  }

  def onNewParameter : Unit = {
    NewParameterModal.show
  }

  def onNewDefinition : Unit = {
    NewDefinitionModal.show
  }

  def onNewModule : Unit = {
    NewModuleModal.show
  }


  //============================================================================================
  // AJAX REQUESTS
  //

  case class GetRequest[A](val address : String)(implicit val aReader : JsonReadable[A, js.Any])
  case class PostRequest[A](val address : String, val request : js.Any)(implicit val aReader : JsonReadable[A, js.Any])

  def doGetRequest[A](getReq : GetRequest[A]) : Future[A] = 
    for {
      xmlReq <- Ajax.get(getReq.address, headers = Seq(("Content-type" -> "application/json")))
    } yield {

      implicit val reader = getReq.aReader

      js.JSON.parse(xmlReq.responseText).as[Error[A]] match {
        case Right(a) => a
        case Left(msg) => {
          Toastr.error(msg)
          throw new Exception("Request returned an error.")
        }
      }
    }

  def doPostRequest[A](postReq : PostRequest[A]) : Future[A] = 
    for {
      xmlReq <- Ajax.post(postReq.address, 
        js.JSON.stringify(postReq.request), 0,
        Seq(("Content-type" -> "application/json")), false)
    } yield {

      implicit val reader = postReq.aReader

      js.JSON.parse(xmlReq.responseText).as[Error[A]] match {
        case Right(a) => a
        case Left(msg) => {
          Toastr.error(msg)
          throw new Exception("Request returned an error.")
        }
      }
    }

  def requestModule(moduleId : String) : Future[String] = {

    val request = 
      PostRequest[String](
        "/new-module",
        lit(
          "moduleId" -> moduleId,
          "address" -> JsJsonWriter.write(activeCheckerAddress)
        )
      )

    doPostRequest(request)

  }

  def requestNewWorksheet(target : dom.Element, panelSize : Int) : Future[JsWorksheet] = {

    implicit val readType = JsWorksheet.WorksheetCreate(target, panelSize)

    val request = 
      GetRequest[JsWorksheet](
        "/new-worksheet"
      )

    doGetRequest(request)

  }

  def refreshWorksheet(worksheet : JsWorksheet) : Future[JsWorksheet] = {

    implicit val readType = JsWorksheet.WorksheetRefresh(worksheet)

    val request = PostRequest[JsWorksheet](
      "/request-worksheet",
      lit("worksheetId" -> worksheet.remoteId)
    )

    doPostRequest(request)

  }

  def requestExtrusion(worksheet : JsWorksheet) : Future[JsWorksheet] = {

    implicit val readType = JsWorksheet.WorksheetRefresh(worksheet)

    val request = 
      PostRequest[JsWorksheet](
        "/extrude-worksheet",
        lit(
          "worksheetId" -> worksheet.remoteId,
          "selectionDescriptor" -> JsJsonWriter.write(worksheet.descriptor)
        )
      )

    doPostRequest(request)

  }

  def requestDrop(worksheet : JsWorksheet) : Future[JsWorksheet] = {

    implicit val readType = JsWorksheet.WorksheetRefresh(worksheet)

    val request = 
      PostRequest[JsWorksheet](
        "/drop-worksheet",
        lit(
          "worksheetId" -> worksheet.remoteId,
          "selectionDescriptor" -> JsJsonWriter.write(worksheet.descriptor)
        )
      )

    doPostRequest(request)

  }

  def requestEnvironment : Future[RoseTree[String, String]] = {

    val request = 
      PostRequest[RoseTree[String, String]](
        "/environment",
        lit(
          "address" -> JsJsonWriter.write(activeCheckerAddress)
        )
      )

    doPostRequest(request)

  }

  def requestParameter(
    worksheetId : Int,
    cellAddress : CellAddress, 
    identString : String,
    isThin : Boolean
  ) : Future[String] = {

    val request = 
      PostRequest[String](
        "/new-parameter",
        lit(
          "worksheetId" -> worksheetId,
          "cellAddress" -> JsJsonWriter.write(cellAddress),
          "identString" -> identString,
          "isThin" -> isThin,
          "checkerAddress" -> JsJsonWriter.write(activeCheckerAddress)
        )
      )

    doPostRequest(request)

  }

  def requestDefinition(
    worksheetId : Int,
    cellAddress : CellAddress, 
    identString : String
  ) : Future[String] = {

    val request = 
      PostRequest[String](
        "/new-definition",
        lit(
          "worksheetId" -> worksheetId,
          "cellAddress" -> JsJsonWriter.write(cellAddress),
          "identString" -> identString,
          "checkerAddress" -> JsJsonWriter.write(activeCheckerAddress)
        )
      )

    doPostRequest(request)

  }

  //============================================================================================
  // MAIN ENTRY POINT
  //

  def main(): Unit = {
    println("Starting Orchard ...")

    val node = new JsModuleNode("Prelude")

    node.cursorsJQ.each((i : js.Any, el : dom.Element) => {
      jQuery(el).find(".cursor-bar").addClass("active")
    })

    jqModuleWrapper.append(node.panelJq)
    val rm = Module(node, Vector.empty)
    rootModule = Some(rm)

    setCursorPosition(Vector.empty, 0)

    val listElement = document.createElement("li")
    jQuery(".worksheet-carousel ul").append(listElement)

    requestNewWorksheet(listElement, 200) onSuccess {
      case worksheet => {

        worksheet.renderAll

        import JCarousel._

        // Let's initialize the worksheet carousel
        jQuery(".worksheet-carousel").jcarousel(lit(
          "vertical" -> true
        )).on("jcarousel:reloadend", (e : JQueryEventObject, c : JCarousel) => {
          jQuery(".worksheet-carousel-pagination").jcarouselPagination("reloadCarouselItems");
        })

        jQuery(".worksheet-carousel-prev").jcarouselControl(lit(
          "target" -> "-=1"
        ))

        jQuery(".worksheet-carousel-next").jcarouselControl(lit(
          "target" -> "+=1"
        ))
        
        jQuery(".worksheet-carousel-pagination").jcarouselPagination(
          lit(
            "item" -> (((page : js.String) => {
              "<a href=\"#" ++ page.toString + "\"><i class=\"fa fa-circle\"></i></a></li>"
            }) : js.Function1[js.String, js.String])
          )
        )

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
