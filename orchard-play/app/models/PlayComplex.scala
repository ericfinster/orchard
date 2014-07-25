/**
  * PlayComplex.scala - A Complex with suitable serialization for Play
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package models

import play.api.libs.json._

import orchard.core.cell._
import orchard.core.util._
import orchard.core.complex._

class PlayComplex[A](seed : NCell[A]) extends MutableSkeletalComplex[A] {

  type CellType = PlayCell

  def newCell(item : A) : PlayCell = new PlayCell(item)

  var topCell : PlayCell =
    seed.regenerateFrom(ComplexGenerator).value

  class PlayCell(var item : A) extends MutableSkeletalCell {

    var canopy : Option[RoseTree[PlayCell, Int]] = None
    var target : Option[PlayCell] = None
    var sources : Option[Vector[PlayCell]] = None
    var container : Option[PlayCell] = None
    
    var incoming : Option[PlayCell] = None
    var outgoing : Option[PlayCell] = None

    // Ummm ... really?
    var skeleton : NCell[CellType] = null

  }

}

object PlayComplex {

  import PlayToOrchard._

  implicit def complexWrites[A](implicit aWrites : Writes[A]) : Writes[PlayComplex[A]] = 
    new Writes[PlayComplex[A]] {
      def writes(complex : PlayComplex[A]) : JsValue = {
        val aWriter = implicitly[JsonWritable[A, JsValue]]
        complex.toJson(PlayJsonWriter, aWriter)
      }
    }

  implicit def complexReads[A](implicit aReads : Reads[A]) : Reads[PlayComplex[A]] = 
    new Reads[PlayComplex[A]] {
      def reads(json : JsValue) : JsResult[PlayComplex[A]] = {
        val aReader = implicitly[JsonReadable[A, JsValue]]

        // Still need to do this by giving a complex implementation which builds
        // itself from a json constructor ...

        ???
      }
    }

}
