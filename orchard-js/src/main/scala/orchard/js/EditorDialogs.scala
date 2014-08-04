/**
  * EditorDialogs.scala - Dialog definitions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.js

import scala.scalajs._
import concurrent.JSExecutionContext.Implicits.queue

import org.scalajs.jquery._

import orchard.js.plugins._

trait EditorDialogs { thisEditor : Editor =>

  object NewModuleModal extends BootstrapModal("orchard-new-module-modal") {

    val inputJq = modalJq.find("#module-name")

    modalJq.find("#module-form").on("submit", () => {

      val moduleName : String = modalJq.find("#module-name").value.asInstanceOf[js.String]

      newModuleRequest(moduleName) onSuccess {
        case moduleHtml : String => {

          println("Got the following html: " ++ moduleHtml)

          // Great, that works.  Now how do we insert it?  The idea is that we use jquery to
          // locate the appropriate insertion point based on some kind of information in 
          // the html.

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

  object ImportModal extends BootstrapModal("orchard-import-modal") {

    val moduleIdJq = modalJq.find("#module-identifier")

    modalJq.find("#import-form").on("submit", () => {

      val moduleId = moduleIdJq.value.as[String]

      hide
      false

    })

    override def onShow = {
      removeKeyHandler
    }

    override def onShown = {
      moduleIdJq.focus()
    }


    override def onHide = {
      installKeyHandler
      moduleIdJq.value("")
    }

  }

  object NewParameterModal extends BootstrapModal("orchard-new-parameter-modal") {

    val inputJq = modalJq.find("#parameter-name")

    modalJq.find("#parameter-form").on("submit", () => {

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

}
