/**
  * Toastr.scala - Toastr Notification Plugin
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.js.plugins

import scala.scalajs._
import js.annotation.JSName

@JSName("toastr")
object Toastr extends js.Object {

  def success(message : js.String) : Unit = ???
  def success(message : js.String, title : js.String) : Unit = ???

  def error(message : js.String) : Unit = ???
  def error(message : js.String, title : js.String) : Unit = ???

  def info(message : js.String) : Unit = ???
  def info(message : js.String, title : js.String) : Unit = ???

  def warning(message : js.String) : Unit = ???
  def warning(message : js.String, title : js.String) : Unit = ???

  val options : js.Dynamic = ???

}
