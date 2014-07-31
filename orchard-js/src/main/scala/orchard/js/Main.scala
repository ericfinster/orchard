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

  jQuery("#new-module-btn").click(() => {
    NewModuleModal.show
  })

  jQuery("#new-parameter-btn").click(() => {
    NewParameterModal.show
  })

  jQuery("#new-defn-btn").click(() => {
    NewDefinitionModal.show
  })

  jQuery("#new-worksheet-btn").click(() => {
    requestNewWorksheet onSuccess {
      case worksheet => activeWorksheet = Some(worksheet)
    }
  })

  jQuery("#extrude-btn").click(() => {
    for {
      worksheet <- activeWorksheet
    } {
      requestExtrusion(worksheet)
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

    override def onHide = {
      // Really we need to check the response status ...
      val moduleId : String = modalJQuery.find("#module-name").value.asInstanceOf[js.String]
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
    }
  }

  object NewParameterModal extends BootstrapModal("orchard-new-parameter-modal") {

    override def onHide = {

      for {
        worksheet <- activeWorksheet
        targetCell <- worksheet.selectionBase
      } {

        val parameterIdent: String = modalJQuery.find("#parameter-name").value.asInstanceOf[js.String]
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
    }
  }

  object NewDefinitionModal extends BootstrapModal("orchard-new-definition-modal") {

    override def onHide = {

      for {
        worksheet <- activeWorksheet
        targetCell <- worksheet.selectionBase
      } {

        val definitionIdent: String = modalJQuery.find("#definition-name").value.asInstanceOf[js.String]
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

  def requestNewWorksheet : Future[JsWorksheet] = {

    val listElement = document.createElement("li")
    jQuery(".worksheet-carousel ul").append(listElement)

    implicit val readType = JsWorksheet.WorksheetCreate(listElement, 200)

    val request = 
      GetRequest[JsWorksheet](
        "/new-worksheet"
      )

    for {
      newWorksheet <- doGetRequest(request)
    } yield {

      newWorksheet.renderAll

      import JQueryCarousel._

      jQuery(".worksheet-carousel").jcarousel("reload", lit())

      jQuery(listElement).
        on("jcarousel:targetin", (e : JQueryEventObject, c : JCarousel) => {
          activeWorksheet = Some(newWorksheet)
        })

      jQuery(".worksheet-carousel").jcarousel("scroll", jQuery(listElement))

      newWorksheet
    } 

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
