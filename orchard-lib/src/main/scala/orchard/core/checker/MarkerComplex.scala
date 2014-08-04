/**
  * MarkerComplex.scala - A Cell complex consisting of Worksheet Markers
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.checker

import orchard.core.cell._
import orchard.core.util._
import orchard.core.complex._

trait CheckerMarkers { thisChecker : Checker =>

  class MarkerComplex(seed : NCell[WorksheetMarker], owner : CheckerWorksheet)
      extends MutableSkeletalComplex[WorksheetMarker]
      with SelectableComplex[WorksheetMarker] {
    thisComplex =>

    type CellType = MarkerComplexCell

    var topCell : MarkerComplexCell =
      seed.regenerateFrom(ComplexGenerator).value

    def newCell(marker : WorksheetMarker) = new MarkerComplexCell(marker)
    override def serializationId = owner.serializationId

    class MarkerComplexCell(var item : WorksheetMarker) extends MutableSkeletalCell {

      var canopy : Option[RoseTree[MarkerComplexCell, Int]] = None
      var target : Option[MarkerComplexCell] = None
      var sources : Option[Vector[MarkerComplexCell]] = None
      var container : Option[MarkerComplexCell] = None
      
      var incoming : Option[MarkerComplexCell] = None
      var outgoing : Option[MarkerComplexCell] = None

      // Ummm ... really?
      var skeleton : NCell[CellType] = null

    }
  }

  object MarkerComplex {

    // val emptyComplexCell : NCell[WorksheetMarker] =
    //   Object(EmptyMarker(true, false)).glob(
    //     PositivePolarityMarker,
    //     NegativePolarityMarker
    //   )

    // def apply() : MarkerComplex =
    //   new MarkerComplex(emptyComplexCell)

    implicit def markerComplexWritable[P] : JsonWritable[MarkerComplex, P] =
      new JsonWritable[MarkerComplex, P] {
        def write(worksheet : MarkerComplex, writer : JsonWriter[P]): P = {
          val markerWriter = implicitly[JsonWritable[WorksheetMarker, P]]
          worksheet.toJson(writer, markerWriter)
        }
      }

  }

}
