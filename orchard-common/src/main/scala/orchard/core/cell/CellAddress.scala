/**
  * CellAddress.scala - Addresses inside a Cell
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.cell

sealed trait CellAddress
sealed trait AddressPrefix extends CellAddress

case object Immediate extends AddressPrefix { 
  override def toString = "*" 
}

case class Target(val prefix : AddressPrefix) extends AddressPrefix {
  override def toString = "Tgt : " ++ prefix.toString
}

case class Source(val prefix : AddressPrefix, val loc : List[Int]) extends CellAddress {
  override def toString = loc.toString ++ " : " ++ prefix.toString
}

object CellAddress {

  import orchard.core.util._

  implicit def cellAddrIsReadable[P] : JsonReadable[CellAddress, P] = 
    new JsonReadable[CellAddress, P] {
      def read(x : P, reader : JsonReader[P]): CellAddress = {
        reader.readString(reader.readObjectField(x, "type")) match {
          case "immediate" => Immediate
          case "target" => {
            val prefix = read(reader.readObjectField(x, "prefix"), reader).asInstanceOf[AddressPrefix]
            Target(prefix)
          }
          case "source" => {
            val listReader = implicitly[JsonReadable[List[Int], P]]
            val prefix = read(reader.readObjectField(x, "prefix"), reader).asInstanceOf[AddressPrefix]
            val loc = listReader.read(reader.readObjectField(x, "loc"), reader)
            Source(prefix, loc)
          }
        }
      }
    }

  implicit def cellAddrIsWritable[P] : JsonWritable[CellAddress, P] = 
    new JsonWritable[CellAddress, P] {
      def write(addr : CellAddress, writer: JsonWriter[P]): P = {
        addr match {
          case Immediate => writer.writeObject(("type" -> writer.writeString("immediate")))
          case Target(prefix) => {
            writer.writeObject(
              ("type" -> writer.writeString("target")),
              ("prefix" -> write(prefix, writer))
            )
          }
          case Source(prefix, loc) => {
            val listWriter = implicitly[JsonWritable[List[Int], P]]

            writer.writeObject(
              ("type" -> writer.writeString("source")),
              ("prefix" -> write(prefix, writer)),
              ("loc" -> listWriter.write(loc, writer))
            )
          }
        }
      }
    }

  

}
