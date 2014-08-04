/**
  * EditorAjax.scala - Ajax routines
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.js

import scala.scalajs._
import concurrent.JSExecutionContext.Implicits.queue

import scala.concurrent.Future

import org.scalajs.dom
import org.scalajs.dom.extensions._
import org.scalajs.jquery._

import orchard.core.util._
import orchard.js.plugins.Toastr

import ErrorM._

trait EditorAjax { thisEditor : Editor =>

  //============================================================================================
  // GENERIC AJAX HELPER
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

  //============================================================================================
  // AJAX REQUESTS
  //



}
