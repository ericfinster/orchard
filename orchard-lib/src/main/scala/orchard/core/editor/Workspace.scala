/**
  * Workspace.scala - A workspace for manipulating expressions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.editor

import orchard.core.cell._
import orchard.core.complex._
import orchard.core.expression._

trait Workspace extends CheckableEnvironment {

  class Worksheet(seed : NCell[Polarity[Option[Expression]]])
      extends AbstractWorksheet(seed)
      with CheckableFramework[Polarity[Option[Expression]]] {

    type CellType = WorksheetCell

    def newCell(item : Polarity[Option[Expression]]) = new WorksheetCell(item)
    def extract(cell : CellType) = new Worksheet(cell.skeleton map (_.item))

    class WorksheetCell(var item : Polarity[Option[Expression]])
        extends AbstractWorksheetCell
        with CheckableCell {

      def expression : Option[Expression] = 
        item match {
          case Neutral(exprOpt) => exprOpt
          case _ => throw new IllegalArgumentException("Tried to get expression from polarized cell.")
        }

      def framework : ExpressionFramework = 
        new ExpressionFramework(skeleton map (_.expression))
    }
  }

  class ExpressionFramework(seed : NCell[Option[Expression]])
      extends AbstractMutableComplex[Option[Expression]](seed)
      with Framework[Option[Expression]]
      with CheckableFramework[Option[Expression]] {

    type CellType = ExpressionFrameworkCell

    def newCell(item : Option[Expression]) = new ExpressionFrameworkCell(item)
    def extract(cell : CellType) = new ExpressionFramework(cell.skeleton map (_.item))
    def emptyItem : Option[Expression] = None

    class ExpressionFrameworkCell(var item : Option[Expression])
        extends AbstractMutableCell
        with FrameworkCell
        with CheckableCell {

      def expression : Option[Expression] = item
    }
  }

}
