/**
  * MarkerComplex.scala - A Complex for Expression Markers
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.typechecker

import orchard.core.cell._
import orchard.core.complex._

class MarkerComplex(seed : NCell[ExpressionMarker]) 
    extends AbstractMutableComplex[ExpressionMarker](seed)
    with SelectableComplex[ExpressionMarker] { thisComplex =>

  type CellType = MarkerCell

  def newCell(item : ExpressionMarker) = new MarkerCell(item)

  class MarkerCell(var item : ExpressionMarker) extends AbstractMutableCell

}
