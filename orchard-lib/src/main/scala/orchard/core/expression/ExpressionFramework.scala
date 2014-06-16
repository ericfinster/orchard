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

  type FrameworkType <: ExpressionFramework
  type CellType <: ExpressionFrameworkCell

  val emptyItem : Option[Expression] = None

  abstract class ExpressionFrameworkCell
      extends AbstractMutableCell
      with FrameworkCell { thisCell : CellType =>

    // If there is an expression here, read the values of its faces back into
    // the lower dimensional part of this framework ...
    def promoteFaces : Unit = 
      for {
        expr <- expression
      } {
        expr.ncell.zip(skeleton).get map {
          case (faceValue, cell) => {
            cell.item = Some(faceValue)
          }
        }
      }
  }

}
