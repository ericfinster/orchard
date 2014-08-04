/**
  * EditorDialogs.scala - Dialog definitions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.js

import scala.scalajs._
import org.scalajs.jquery._

import orchard.js.plugins._

trait EditorDialogs { thisEditor : Editor =>

  object NewModuleModal extends BootstrapModal("orchard-new-module-modal") {

    val inputJq = modalJq.find("#module-name")

    modalJq.find("#module-form").on("submit", () => {

      // Really we need to check the response status ...
      val moduleId : String = modalJq.find("#module-name").value.asInstanceOf[js.String]

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
