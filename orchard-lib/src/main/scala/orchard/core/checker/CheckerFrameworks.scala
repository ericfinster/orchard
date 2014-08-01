/**
  * CheckerFrameworks.scala - Frameworks to use in the checker
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.checker 

import orchard.core.cell._
import orchard.core.complex._

trait CheckerFrameworks { thisChecker : Checker =>

  class CheckerFramework(seed : NCell[Option[Expression]]) 
      extends Framework[Option[Expression]](seed) {

    type FrameworkType = CheckerFramework
    type CellType = CheckerFrameworkCell

    def extract(cell : CheckerFrameworkCell) : CheckerFramework = 
      new CheckerFramework(cell.toNCell)

    def newCell(item : Option[Expression]) = 
      new CheckerFrameworkCell(item)

    var topCell : CheckerFrameworkCell =
      seed.regenerateFrom(ComplexGenerator).value

    val stabilityLevel : Option[Int] = None
    val invertibilityLevel : Option[Int] = None
    val unicityLevel : Option[Int] = None

    class CheckerFrameworkCell(var item : Option[Expression]) extends FrameworkCell

  }


}
