/**
  * Module.scala - Modules
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

import orchard.core.cell._
import orchard.core.complex._

trait Module extends ModuleEntry {

  type EntryType <: ModuleEntry

  def entries : Seq[EntryType]

  def stabilityLevel : Option[Int]
  def invertibilityLevel : Option[Int]
  def unicityLevel : Option[Int]

  //============================================================================================
  // WORKSHEETS
  //

  class Worksheet(seed : NCell[Polarity[Option[Expression]]])
      extends AbstractWorksheet(seed) {

    type CellType = WorksheetCell

    def stabilityLevel : Option[Int] = None // module.stabilityLevel
    def invertibilityLevel : Option[Int] = None // module.invertibilityLevel
    def unicityLevel : Option[Int] = None // module.unicityLevel

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

  //============================================================================================
  // FRAMEWORKS
  //

  class ModuleFramework(seed : NCell[Option[Expression]]) 
      extends ExpressionFramework(seed) {

    type CellType = ModuleFrameworkCell

    def stabilityLevel : Option[Int] = None // module.stabilityLevel
    def invertibilityLevel : Option[Int] = None // module.invertibilityLevel
    def unicityLevel : Option[Int] = None // module.unicityLevel

    def newCell(item : Option[Expression]) =
      new ModuleFrameworkCell(item)

    def extract(cell : CellType) =
      new ModuleFramework(cell.skeleton map (_.item))

    class ModuleFrameworkCell(var item : Option[Expression])
        extends ExpressionFrameworkCell(item)

  }

}
