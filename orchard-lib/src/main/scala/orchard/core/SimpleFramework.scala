/**
  * SimpleFramework.scala - An implementation of the ExpressionFramework trait
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.mutable.WeakHashMap
import scala.collection.mutable.Buffer

class SimpleFramework(seed : NCell[Option[Expression]]) 
    extends AbstractMutableComplex[Option[Expression]](seed) 
    with ExpressionFramework[Option[Expression]] {

  type CellType = SimpleFrameworkCell

  def newCell(item : Option[Expression]) = new SimpleFrameworkCell(item)

  def toExpressionCell : NCell[Expression] = topCell.toExpressionCell

  def variables : Seq[NCell[Expression]] = {
    val buf = Buffer.empty[NCell[Expression]]
    forAllCells (cell => if (cell.isVariable) { buf += cell.toExpressionCell })
    buf
  }

  def fillers : Seq[NCell[Expression]] = {
    val buf = Buffer.empty[NCell[Expression]]
    forAllCells (cell => if (cell.isFiller) { buf += cell.toExpressionCell })
    buf
  }

  def fillerFaces : Seq[NCell[Expression]] = {
    val buf = Buffer.empty[NCell[Expression]]
    forAllCells (cell => if (cell.isFillerFace) { buf += cell.toExpressionCell })
    buf
  }

  override def clone : SimpleFramework = new SimpleFramework(this.toCell)

  class SimpleFrameworkCell(var item : Option[Expression])
      extends AbstractMutableCell with ExpressionFrameworkCell {

    def exprItem = item

    def toExpressionCell : NCell[Expression] = skeleton map (_.item.get)

    def isVariable : Boolean = 
      item match {
        case Some(Variable(_, _)) => true
        case _ => false
      }

    def isFiller : Boolean = 
      item match {
        case Some(Filler(_)) => true
        case _ => false
      }

    def isFillerFace : Boolean =
      item match {
        case Some(FillerFace(_, _, _)) => true
        case _ => false
      }
  }

}

object SimpleFramework {

  def apply(seed : NCell[Expression]) : SimpleFramework = 
    new SimpleFramework(seed map (Some(_)))

}
