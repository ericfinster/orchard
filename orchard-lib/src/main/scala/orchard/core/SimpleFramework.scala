/**
  * SimpleFramework.scala - An implementation of the ExpressionFramework trait
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

class SimpleFramework(seed : NCell[Option[Expression]]) 
    extends AbstractFramework(seed) with ExpressionFramework[Option[Expression]] {

  type CellType = SimpleFrameworkCell

  def newCell(item : Option[Expression]) = new SimpleFrameworkCell(item)

  override def clone : SimpleFramework = new SimpleFramework(this.toCell)

  class SimpleFrameworkCell(item : Option[Expression]) extends AbstractFrameworkCell(item)

}

object SimpleFramework {

  def apply(seed : NCell[Expression]) : SimpleFramework = 
    new SimpleFramework(seed map (Some(_)))

}
