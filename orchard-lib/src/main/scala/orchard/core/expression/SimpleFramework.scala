/**
  * SimpleFramework.scala - A workspace independent framework implementation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

import orchard.core.cell._
import orchard.core.complex._

class SimpleFramework(seed : NCell[Option[Expression]])
    extends AbstractMutableComplex[Option[Expression]](seed)
    with Framework[Option[Expression]] {

  type CellType = SimpleFrameworkCell

  def newCell(item : Option[Expression]) = new SimpleFrameworkCell(item)
  def extract(cell : CellType) = new SimpleFramework(cell.skeleton map (_.item))
  def emptyItem : Option[Expression] = None

  class SimpleFrameworkCell(var item : Option[Expression])
      extends AbstractMutableCell
      with FrameworkCell {

    def expression : Option[Expression] = item

  }
}

