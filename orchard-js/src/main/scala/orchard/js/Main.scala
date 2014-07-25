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

  val mainDiv : JQuery = jQuery("#orchard-main")

  jQuery("#newModuleButton").click((() => { 
    NewModuleModal.show
  }) : js.Function)

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


    requestModule("Prelude") onSuccess { 
      case xmlReq => {
        val desc = new JsModuleDescription("Prelude", xmlReq.responseText, Vector.empty)
        mainDiv.append(desc.panelDiv)
        val rm = Module(desc, Vector.empty)
        rootModule = Some(rm)
      }
    }

  }

  def refreshModuleAddresses : Unit = 
    for {
      root <- rootModule
    } {
      ModuleZipper(root, Nil) foreachWithAddress {
        case (addr, desc) => {
          println("Setting address of " ++ desc.name ++ " to " ++ addr.toString)
          desc.address = addr
        }
      }
    }

  def processModuleHtml(moduleId : String, html : String) : Unit = {

    // Now, what's the idea here?

    // Okay, we are going to set the various even handlers and whatnot.  But the point is that we
    // are going to look at the current div pointed to by the module pointer and add the processed
    // data to that div.

    // val moduleDesc = new JsModuleDescription(moduleId, html)


    // val parentDiv = 
    //   modulePointer match {
    //     case None => mainDiv
    //     case Some(ModuleZipper(focus, _)) => focus.desc.asInstanceOf[JsModuleDescription].moduleDiv
    //   }

    // parentDiv.append(html)

    // Now we need to actually create the module. But we need the name ...

    // val module = Module(new JsModuleDescription(moduleId, parentDiv.find(".panel-body")), Vector.empty)

    // Now what we want to do is set the module pointer to point to the new module.  I guess
    // eventuall this should be done with select somehow, but for now ...

    // modulePointer = 
    //   modulePointer match {
    //     case None => Some(ModuleZipper(module, Nil))
    //     case Some(ptr) => Some(ptr.appendEntry(module).get)
    //   }

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
