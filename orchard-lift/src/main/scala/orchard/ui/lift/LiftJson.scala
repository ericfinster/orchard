/**
  * LiftJson.scala - Reader and Writer for Server side Json
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.lift

import orchard.core.util._

import net.liftweb.json.JsonAST._

// Okay, these are a bit sloppy, but ....

object LiftJsonWriter extends JsonWriter[JValue] {

  def writeNull : JValue = JNull
  def writeBoolean(b : Boolean) : JValue = JBool(b)
  def writeDouble(x : Double) : JValue = JDouble(x)
  def writeString(s : String) : JValue = JString(s)
  def writeArray(elems : JValue*) : JValue = JArray(elems.toList)
  def writeObject(fields : (String, JValue)*) : JValue = 
    JObject(fields.toList map { case (name, value) => JField(name, value) })
  
}

object LiftJsonReader extends JsonReader[JValue] {

  def isUndefined(x : JValue) : Boolean = x == JNothing
  def isNull(x : JValue) : Boolean = x == JNull

  def readBoolean(x : JValue) : Boolean = 
    (x: @unchecked) match {
      case JBool(b) => b
    }

  def readNumber(x : JValue) : Double =
    (x: @unchecked) match {
      case JDouble(d) => d
    }

  def readString(x : JValue) : String =
    (x: @unchecked) match {
      case JString(s) => s
    }

  def readArrayLength(x : JValue) : Int = 
    (x: @unchecked) match {
      case JArray(l) => l.length
    }

  def readArrayElement(x : JValue, i : Int) : JValue = 
    (x: @unchecked) match {
      case JArray(l) => l(i)
    }

  def readObjectField(x : JValue, field : String) : JValue = 
    (x: @unchecked) match {
      case JObject(flds) => {
        (flds find (_.name == field)).get
      }
    }

}
