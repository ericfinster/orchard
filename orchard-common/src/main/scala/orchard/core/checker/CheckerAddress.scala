/**
  * CheckerAddress.scala - A case class for addressing in the checker
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.checker

import orchard.core.util._

case class CheckerAddress(val moduleAddress : Vector[Int], val cursorOffset : Int) 

object CheckerAddress {

  implicit def checkerAddrIsReadable[P] : JsonReadable[CheckerAddress, P] =
    new JsonReadable[CheckerAddress, P] {
      def read(x : P, reader : JsonReader[P]) : CheckerAddress = {
        val vectorReader = implicitly[JsonReadable[Vector[Int], P]]
        val moduleAddress = vectorReader.read(reader.readObjectField(x, "moduleAddress"), reader)
        val cursorOffset = reader.readNumber(reader.readObjectField(x, "cursorOffset"))
        CheckerAddress(moduleAddress, cursorOffset.toInt)
      }
    }

  implicit def checkerAddrIsWritable[P] : JsonWritable[CheckerAddress, P] =
    new JsonWritable[CheckerAddress, P] {
      def write(ca : CheckerAddress, writer : JsonWriter[P]) : P = {
        val vectorWriter = implicitly[JsonWritable[Vector[Int], P]]

        writer.writeObject(
          ("moduleAddress" -> vectorWriter.write(ca.moduleAddress, writer)),
          ("cursorOffset" -> writer.writeDouble(ca.cursorOffset))
        )
      }
    }

}
