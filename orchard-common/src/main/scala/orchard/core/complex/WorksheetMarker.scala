/**
  * WorksheetMarker.scala - Marker's to put on expression cells
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.complex

sealed trait WorksheetMarker {

  def name : String
  def styleString : String

  def isEmpty : Boolean
  def isShell : Boolean
  def isNeutral : Boolean
  def isExposedNook : Boolean
  def isSelectable : Boolean

}

sealed trait PolarityMarker extends WorksheetMarker {

  def styleString = "polarized"

  def isEmpty = false
  def isShell = false
  def isExposedNook = false
  def isSelectable = false

}

case object PositivePolarityMarker extends PolarityMarker { def name = "+" ; def isNeutral = false }
case object NegativePolarityMarker extends PolarityMarker { def name = "-" ; def isNeutral = false }

case class EmptyMarker(val isShell : Boolean, val isExposedNook : Boolean) extends WorksheetMarker {

  def name = "empty"
  def styleString = "empty"

  def isEmpty = true
  def isNeutral = true
  def isSelectable = true

}

case class ReferenceMarker(val name : String, val styleString : String) extends WorksheetMarker {

  def isEmpty = false
  def isShell = false
  def isNeutral = true
  def isExposedNook = false
  def isSelectable = true

}

object WorksheetMarker {

  import orchard.core.util._

  implicit def markerIsReadable[P] : JsonReadable[WorksheetMarker, P] =
    new JsonReadable[WorksheetMarker, P] {
      def read(x : P, reader : JsonReader[P]) : WorksheetMarker = {

        val name = reader.readString(reader.readObjectField(x, "name"))
        val styleString = reader.readString(reader.readObjectField(x, "styleString"))
        val isEmpty = reader.readBoolean(reader.readObjectField(x, "isEmpty"))
        val isShell = reader.readBoolean(reader.readObjectField(x, "isShell"))
        val isNeutral = reader.readBoolean(reader.readObjectField(x, "isNeutral"))
        val isExposedNook = reader.readBoolean(reader.readObjectField(x, "isExposedNook"))
        val isSelectable = reader.readBoolean(reader.readObjectField(x, "isSelectable"))

        reader.readString(reader.readObjectField(x, "type")) match {
          case "positive" => PositivePolarityMarker
          case "negative" => NegativePolarityMarker
          case "empty" => EmptyMarker(isShell, isExposedNook)
          case "ref" => ReferenceMarker(name, styleString)
        }

      }
    }

  implicit def markerIsWritable[P] : JsonWritable[WorksheetMarker, P] =
    new JsonWritable[WorksheetMarker, P] {
      def write(wm : WorksheetMarker, writer : JsonWriter[P]) : P = {

        val markerType = 
          wm match {
            case PositivePolarityMarker => "positive"
              case NegativePolarityMarker => "negative"
              case _ : EmptyMarker => "empty"
              case _ : ReferenceMarker => "ref"
            }

        val fields = List(
          ("type" -> writer.writeString(markerType)),
          ("name" -> writer.writeString(wm.name)),
          ("styleString" -> writer.writeString(wm.styleString)),
          ("isEmpty" -> writer.writeBoolean(wm.isEmpty)),
          ("isShell" -> writer.writeBoolean(wm.isShell)),
          ("isNeutral" -> writer.writeBoolean(wm.isNeutral)),
          ("isExposedNook" -> writer.writeBoolean(wm.isExposedNook)),
          ("isSelectable" -> writer.writeBoolean(wm.isSelectable))
        )

        writer.writeObject(fields : _*)
      }
    }

}
