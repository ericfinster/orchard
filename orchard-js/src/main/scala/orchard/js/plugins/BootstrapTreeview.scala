/**
  * BootstrapTreeview.scala - Tree view plugin for bootstrap
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.js.plugins

import scala.language.implicitConversions

import scala.scalajs._
import org.scalajs.jquery.JQuery

trait BootstrapTreeview extends JQuery {

  def treeview(data : js.Any) : BootstrapTreeview = ???

}

object BootstrapTreeview {

  implicit def jq2Treeview(jq : JQuery): BootstrapTreeview = 
    jq.asInstanceOf[BootstrapTreeview]

}
