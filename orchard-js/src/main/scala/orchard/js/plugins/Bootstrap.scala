/**
  * Bootstrap.scala - Implementation of various Bootstrap API elements
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.js.plugins

import scala.language.implicitConversions

import scala.scalajs._
import org.scalajs.jquery._

trait BootstrapJQuery extends JQuery {

  def modal() : BootstrapJQuery = ???
  def modal(conf : js.Object) : BootstrapJQuery = ???
  def modal(cmd : String) : BootstrapJQuery = ???

}

object BootstrapJQuery {

  implicit def jq2Bootstrap(jq : JQuery) : BootstrapJQuery =
    jq.asInstanceOf[BootstrapJQuery]

}

abstract class BootstrapModal(id : String) {

  import BootstrapJQuery._

  val modalJq = jQuery("#" ++ id)

  modalJq.modal(js.Dynamic.literal())

  modalJq.on("show.bs.modal", ((e : js.Any) => {
    onShow
  }) : js.Function1[js.Any, Unit])

  modalJq.on("shown.bs.modal", ((e : js.Any) => {
    onShown
  }) : js.Function1[js.Any, Unit])

  modalJq.on("hide.bs.modal", ((e : js.Any) => {
    onHide
  }) : js.Function1[js.Any, Unit])

  modalJq.on("hidden.bs.modal", ((e : js.Any) => {
    onHidden
  }) : js.Function1[js.Any, Unit])


  def onShow : Unit = ()
  def onShown : Unit = ()

  def onHide : Unit = ()
  def onHidden : Unit = ()

  def show : Unit = {
    modalJq.modal("show")
  }

  def hide : Unit = {
    modalJq.modal("hide")
  }

}

