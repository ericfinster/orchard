/**
  * EditorImplicits.scala - Repository for implict setup of the editor
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.js

import scala.scalajs._

import org.scalajs.dom
import org.scalajs.jquery._

import orchard.core.util._

trait EditorImplicits { thisEditor : Editor =>

  implicit class JsAnyOps(x : js.Any) {

    def as[A](implicit aReader : JsonReadable[A, js.Any]) : A = 
      aReader.read(x, JsJsonReader)

  }

  implicit def jqEventHandlerAction(handler : JQueryEventObject => Unit) : js.Function1[JQueryEventObject, js.Any] =
    ((e : JQueryEventObject) => { handler(e) ; (true : js.Any) }) : js.Function1[JQueryEventObject, js.Any]

  implicit def jqEachIterator(handler : (js.Any, dom.Element) => Unit) : js.Function2[js.Any, dom.Element, js.Any] = 
    ((i : js.Any, el : dom.Element) => { handler(i, el) ; (true : js.Any) }) : js.Function2[js.Any, dom.Element, js.Any]

}
