/**
  * ExpressionFramework.scala - An abstract class for frameworks based on Option[Expressions]'s
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

import orchard.core.cell._
import orchard.core.complex._

abstract class ExpressionFramework(seed : NCell[Option[Expression]])
    extends AbstractMutableComplex[Option[Expression]](seed)
    with Framework[Option[Expression]] {

  type CellType <: ExpressionFrameworkCell

  val emptyItem : Option[Expression] = None

  abstract class ExpressionFrameworkCell(item : Option[Expression])
      extends AbstractMutableCell
      with FrameworkCell { thisCell : CellType =>

    def expression : Option[Expression] = item

  }

}
