/**
  * SectionEntry.scala - Common superclass for definitions and sections
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.editor

import orchard.core.cell._
import orchard.core.complex._
import orchard.core.expression._

trait SectionEntry {

  def module : Module

  class Worksheet(seed : NCell[Polarity[Option[Expression]]])
      extends AbstractWorksheet(seed) {

    type CellType = WorksheetCell

    def stabilityLevel : Option[Int] = module.stabilityLevel
    def invertibilityLevel : Option[Int] = module.invertibilityLevel
    def unicityLevel : Option[Int] = module.unicityLevel

    def newCell(item : Polarity[Option[Expression]]) = 
      new WorksheetCell(item)

    def extract(cell : CellType) =
      new Worksheet(cell.skeleton map (_.item))

    class WorksheetCell(var item : Polarity[Option[Expression]]) 
        extends AbstractWorksheetCell {

      def expression : Option[Expression] = 
        item match {
          case Neutral(exprOpt) => exprOpt
          case _ => ???
        }

    }
  }

  class SectionFramework(seed : NCell[Option[Expression]]) 
      extends ExpressionFramework(seed) {

    type CellType = SectionFrameworkCell

    def stabilityLevel : Option[Int] = module.stabilityLevel
    def invertibilityLevel : Option[Int] = module.invertibilityLevel
    def unicityLevel : Option[Int] = module.unicityLevel

    def newCell(item : Option[Expression]) =
      new SectionFrameworkCell(item)

    def extract(cell : CellType) =
      new SectionFramework(cell.skeleton map (_.item))

    class SectionFrameworkCell(var item : Option[Expression])
        extends ExpressionFrameworkCell(item)

  }

}


