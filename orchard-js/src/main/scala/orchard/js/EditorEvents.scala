/**
  * EditorEvents.scala - Event Handling
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.js

import scala.scalajs._

import org.scalajs.dom
import org.scalajs.jquery._

trait EditorEvents { thisEditor : Editor =>

  jQuery("#new-module-btn").click(() => { onNewModule })
  jQuery("#new-import-btn").click(() => { onImport })
  jQuery("#new-parameter-btn").click(() => { onNewParameter })
  jQuery("#new-defn-btn").click(() => { onNewDefinition })
  jQuery("#new-worksheet-btn").click(() => { onNewWorksheet })
  jQuery("#extrude-btn").click(() => { onExtrude })
  jQuery("#drop-btn").click(() => { onDrop })
  jQuery("#paste-btn").click(() => { onPaste })

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
        case 73 => onImport          // i
        case _ => ()
      }

      // println("Keycode: " ++ keyCode.toString)

      true
    }

  def installKeyHandler : Unit = {
    jQuery(dom.document).on("keydown", keyHandler)
  }

  def removeKeyHandler : Unit = {
    jQuery(dom.document).off("keydown", keyHandler)
  }

  //============================================================================================
  // EVENT HANDLERS
  //

  def onNewModule : Unit = NewModuleModal.show
  def onNewParameter : Unit = NewParameterModal.show
  def onNewDefinition : Unit = NewDefinitionModal.show
  def onNewWorksheet : Unit = ()
  def onImport : Unit = ()
  def onExtrude : Unit = ()
  def onDrop : Unit = ()
  def onPaste : Unit = ()

}
