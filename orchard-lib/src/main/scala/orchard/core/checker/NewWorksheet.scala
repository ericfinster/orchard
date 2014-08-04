/**
  * NewWorksheet.scala - A new Worksheet implementation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.checker

import orchard.core.cell._
import orchard.core.util._
import orchard.core.complex._

import ErrorM._

trait NewWorksheetTrait { thisChecker : Checker with NewFrameworkTrait =>

  sealed trait WorksheetEntry extends FrameworkEntry

  case class Neutral(entry : SimpleEntry) extends WorksheetEntry {
    def isEmpty = entry.isEmpty
    def isThin = entry.isThin
  }

  case object Positive extends WorksheetEntry {
    def isEmpty = true
    def isThin = checkerFail("Thin request on positive cell")
  }

  case object Negative extends WorksheetEntry {
    def isEmpty = true
    def isThin = checkerFail("Thin request on negative cell")
  }

  trait Worksheet extends Framework[WorksheetEntry] {
  }

}
