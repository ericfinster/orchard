/**
  * PerfectSrollbar.scala - Perfect Srollbar API exposure
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.js.plugins

import scala.language.implicitConversions

import scala.scalajs._
import org.scalajs.jquery.JQuery

trait PerfectScrollbar extends JQuery {

  def perfectScrollbar() : Unit = ???
  def perfectScrollbar(str : String) : Unit = ???

}

object PerfectScrollbar {

  implicit def jq2Perfect(jq : JQuery): PerfectScrollbar = 
    jq.asInstanceOf[PerfectScrollbar]

}
