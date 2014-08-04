/**
  * EditorUI.scala - UI Routines
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.js

import org.scalajs.dom
import org.scalajs.jquery._

import orchard.js.plugins.Toastr

trait EditorUI { thisEditor : Editor =>

  jQuery(dom.window).resize(() => { doLayout })

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

}
