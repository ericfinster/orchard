/**
  * Json.scala - Json Traits and TypeClasses
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.util

import scala.reflect.ClassTag

trait JsonReader[P] {

  def isUndefined(x : P) : Boolean
  def isNull(x : P) : Boolean
  def readBoolean(x : P) : Boolean
  def readNumber(x : P) : Double
  def readString(x : P) : String
  def readArrayLength(x : P) : Int
  def readArrayElement(x : P, i : Int) : P
  def readObjectField(x : P, field : String) : P

}

trait JsonReadable[A, P] {
  def read(x : P, reader : JsonReader[P]) : A
}

object JsonReadable {

  implicit def boolIsReadable[P] : JsonReadable[Boolean, P] = 
    new JsonReadable[Boolean, P] {
      def read(x : P, reader : JsonReader[P]) : Boolean = 
        reader.readBoolean(x)
    }

  implicit def intIsReadable[P] : JsonReadable[Int, P] =
    new JsonReadable[Int, P] {
      def read(x : P, reader : JsonReader[P]) : Int =
        reader.readNumber(x).toInt
    }

  implicit def doubleIsReadable[P] : JsonReadable[Double, P] =
    new JsonReadable[Double, P] {
      def read(x : P, reader : JsonReader[P]) : Double =
        reader.readNumber(x)
    }

  implicit def stringIsReadable[P] : JsonReadable[String, P] =
    new JsonReadable[String, P] {
      def read(x : P, reader : JsonReader[P]) : String =
        reader.readString(x)
    }


  implicit def vectorIsReadable[A, P](implicit ev : JsonReadable[A, P]) : JsonReadable[Vector[A], P] = 
    new JsonReadable[Vector[A], P] {
      def read(x : P, reader : JsonReader[P]) : Vector[A] = {
        val vectorLength = reader.readArrayLength(x)

        val vectorElems = 
          for {
            i <- Range(0, vectorLength)
          } yield {
            ev.read(reader.readArrayElement(x, i), reader)
          }

        vectorElems.toVector
      }
    }


  implicit def arrayIsReadable[A : ClassTag, P](implicit ev : JsonReadable[A, P]) : JsonReadable[Array[A], P] = 
    new JsonReadable[Array[A], P] {
      def read(x : P, reader : JsonReader[P]) : Array[A] = {
        val arrayLength = reader.readArrayLength(x)

        val arrayElems = 
          for {
            i <- Range(0, arrayLength)
          } yield {
            ev.read(reader.readArrayElement(x, i), reader)
          }

        arrayElems.toArray
      }
    }

  implicit def optionIsReadable[A, P](implicit ev : JsonReadable[A, P]) : JsonReadable[Option[A], P] = 
    new JsonReadable[Option[A], P] {
      def read(x : P, reader : JsonReader[P]) : Option[A] = {
        reader.readString(
          reader.readObjectField(x, "type")
        ) match {
          case "none" => None
          case "some" => {
            val valueObj = reader.readObjectField(x, "value")
            Some(ev.read(valueObj, reader))
          }
        }
      }
    }

}

trait JsonWriter[P] {

  def writeNull : P
  def writeBoolean(b : Boolean) : P
  def writeDouble(x : Double) : P
  def writeString(s : String) : P
  def writeArray(elems : P*) : P
  def writeObject(fields : (String, P)*) : P

}

trait JsonWritable[A, P] {
  def write(a : A, writer : JsonWriter[P]) : P
}

object JsonWritable {

  implicit def boolIsWritable[P] : JsonWritable[Boolean, P] = 
    new JsonWritable[Boolean, P] {
      def write(b : Boolean, writer : JsonWriter[P]) : P = {
        writer.writeBoolean(b)
      }
    }

  implicit def intIsWritable[P] : JsonWritable[Int, P] = 
    new JsonWritable[Int, P] {
      def write(b : Int, writer : JsonWriter[P]) : P = {
        writer.writeDouble(b.toDouble)
      }
    }

  implicit def doubleIsWritable[P] : JsonWritable[Double, P] = 
    new JsonWritable[Double, P] {
      def write(b : Double, writer : JsonWriter[P]) : P = {
        writer.writeDouble(b)
      }
    }

  implicit def stringIsWritable[P] : JsonWritable[String, P] = 
    new JsonWritable[String, P] {
      def write(b : String, writer : JsonWriter[P]) : P = {
        writer.writeString(b)
      }
    }

  implicit def arrayIsWritable[A : ClassTag, P](implicit ev : JsonWritable[A, P]) : JsonWritable[Array[A], P] = 
    new JsonWritable[Array[A], P] {
      def write(arr : Array[A], writer : JsonWriter[P]) : P = {
        writer.writeArray(arr map (ev.write(_, writer)) : _*)
      }
    }

  implicit def optionIsWritable[A, P](implicit ev : JsonWritable[A, P]) : JsonWritable[Option[A], P] =
    new JsonWritable[Option[A], P] {
      def write(aOpt : Option[A], writer : JsonWriter[P]) : P = {
        aOpt match {
          case None => {
            writer.writeObject(
              ("type" -> writer.writeString("none"))
            )
          }
          case Some(a) => {
            writer.writeObject(
              ("type" -> writer.writeString("some")),
              ("value" -> ev.write(a, writer))
            )
          }
        }
      }
    }

}
