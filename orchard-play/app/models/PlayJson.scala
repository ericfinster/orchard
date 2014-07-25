/**
  * PlayJson.scala - Json Implementation for Play
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package models

import play.api.libs.json._

import orchard.core.util._

// Again, these can be greatly improved by using better attributes of
// the play json encoding, etc ...

object OrchardToPlay {

  implicit def readableReads[A](implicit reader : JsonReadable[A, JsValue]) : Reads[A] =
    new Reads[A] {
      def reads(json : JsValue) : JsResult[A] = {
        JsSuccess(reader.read(json, PlayJsonReader))
      }
    }

  implicit def writableWrites[A](implicit writer : JsonWritable[A, JsValue]) : Writes[A] =
    new Writes[A] {
      def writes(a : A): JsValue = {
        writer.write(a, PlayJsonWriter)
      }
    }

}

object PlayToOrchard {

  implicit def writesIsWritable[A](implicit w : Writes[A]) : JsonWritable[A, JsValue] = 
    new JsonWritable[A, JsValue] {
      def write(a : A, writer : JsonWriter[JsValue]) : JsValue = 
        Json.toJson(a)
    }

  implicit def readsIsReadable[A](implicit r : Reads[A]) : JsonReadable[A, JsValue] = 
    new JsonReadable[A, JsValue] {
      def read(x : JsValue, reader : JsonReader[JsValue]) : A = {
        println("In Play read.")
        x.as[A]
      }
    }

}

object PlayJsonWriter extends JsonWriter[JsValue] {

  def writeNull : JsValue = JsNull
  def writeBoolean(b : Boolean) : JsValue = JsBoolean(b)
  def writeDouble(x : Double) : JsValue = JsNumber(x)
  def writeString(s : String) : JsValue = JsString(s)
  def writeArray(elems : JsValue*) : JsValue = JsArray(elems.toSeq)
  def writeObject(fields : (String, JsValue)*) : JsValue = JsObject(fields.toSeq)
  
}

object PlayJsonReader extends JsonReader[JsValue] {

  def isUndefined(x : JsValue) : Boolean = x.isInstanceOf[JsUndefined]
  def isNull(x : JsValue) : Boolean = x == JsNull

  def readBoolean(x : JsValue) : Boolean =
    (x: @unchecked) match {
      case JsBoolean(b) => b
    }

  def readNumber(x : JsValue) : Double =
    (x: @unchecked) match {
      case JsNumber(d) => d.toDouble
    }

  def readString(x : JsValue) : String =
    (x: @unchecked) match {
      case JsString(s) => s
    }

  def readArrayLength(x : JsValue) : Int =
    (x: @unchecked) match {
      case JsArray(l) => l.length
    }

  def readArrayElement(x : JsValue, i : Int) : JsValue =
    (x: @unchecked) match {
      case JsArray(l) => l(i)
    }

  def readObjectField(x : JsValue, field : String) : JsValue =
    (x: @unchecked) match {
      case JsObject(flds) => {
        (flds find (_._1 == field)).get._2
      }
    }

}



