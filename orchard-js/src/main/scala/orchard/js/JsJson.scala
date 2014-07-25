/**
  * JsJson.scala - Json Reader/Writer Implementation for OrchardJS
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.js

import orchard.core.util._
import scala.scalajs._

object JsJsonWriter extends JsonWriter[js.Any] {

  def writeNull : js.Any = null
  def writeBoolean(b : Boolean) : js.Any = b
  def writeDouble(x : Double) : js.Any = x
  def writeString(s : String) : js.Any = s
  def writeArray(elems : js.Any*) : js.Any = {
    // I think there is a bug which makes the obvious thing not work ...
    // js.Array(elems)
    val arr = new js.Array[js.Any](elems.length)

    for {
      (e, i) <- elems.zipWithIndex
    } {
      arr(i) = e
    }

    arr
  }
  def writeObject(fields : (String, js.Any)*) : js.Any = {
    val result = js.Dictionary.empty[js.Any]
    for ((prop, value) <- fields)
      result(prop) = value
    result
  }

}

object JsJsonReader extends JsonReader[js.Any] {

  def isUndefined(x: js.Any): Boolean = x.isInstanceOf[js.Undefined]
  def isNull(x: js.Any): Boolean = x eq null
  def readBoolean(x: js.Any): Boolean = x.asInstanceOf[js.Boolean]
  def readNumber(x: js.Any): Double = x.asInstanceOf[js.Number]
  def readString(x: js.Any): String = x.asInstanceOf[js.String]
  def readArrayLength(x: js.Any): Int = x.asInstanceOf[js.Array[_]].length.toInt
  def readArrayElement(x: js.Any, index: Int): js.Any =
    x.asInstanceOf[js.Array[js.Any]].apply(index)
  def readObjectField(x: js.Any, field: String): js.Any =
    x.asInstanceOf[js.Dictionary[js.Any]].apply(field)

}
