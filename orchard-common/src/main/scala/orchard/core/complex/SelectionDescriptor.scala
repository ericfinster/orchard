/**
  * SelectionDescriptor.scala - A simple class for serialization of selection information
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.complex

import orchard.core.cell.CellAddress

case class SelectionDescriptor(val base : Option[CellAddress], val cells : List[CellAddress])

object SelectionDescriptor {

  import orchard.core.util._

  implicit def descIsReadable[P] : JsonReadable[SelectionDescriptor, P] = 
    new JsonReadable[SelectionDescriptor, P] {
      def read(x : P, reader : JsonReader[P]): SelectionDescriptor = {
        val addrReader = implicitly[JsonReadable[Option[CellAddress], P]]
        val listAddrReader = implicitly[JsonReadable[List[CellAddress], P]]

        val base = addrReader.read(reader.readObjectField(x, "base"), reader)
        val cells = listAddrReader.read(reader.readObjectField(x, "cells"), reader)

        SelectionDescriptor(base, cells)
      }
    }

  implicit def descIsWritable[P] : JsonWritable[SelectionDescriptor, P] = 
    new JsonWritable[SelectionDescriptor, P] {
      def write(desc : SelectionDescriptor, writer : JsonWriter[P]): P = {
        val addrWriter = implicitly[JsonWritable[Option[CellAddress], P]]
        val listAddrWriter = implicitly[JsonWritable[List[CellAddress], P]]

        writer.writeObject(
          ("base" -> addrWriter.write(desc.base, writer)),
          ("cells" -> listAddrWriter.write(desc.cells, writer))
        )
      }
    }

}
