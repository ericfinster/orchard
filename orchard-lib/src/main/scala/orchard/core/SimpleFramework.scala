/**
  * SimpleFramework.scala - An implementation of the ExpressionFramework trait
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

import scala.collection.mutable.WeakHashMap

class SimpleFramework(seed : NCell[Option[Expression]]) 
    extends AbstractMutableComplex[Option[Expression]](seed) with ExpressionFramework[Option[Expression]] {

  type CellType = SimpleFrameworkCell

  def newCell(item : Option[Expression]) = new SimpleFrameworkCell(item)

  // If this is an exposed nook, return a minimal amount of information to
  // reconstruct it in a given environment
  def getIdNook : (RoseTree[Option[String], Option[String]], Option[String]) =
    topCell.skeleton.cell match {
      case Composite(_, srcTree, tgtExpr, _) => {
        srcTree.dimension match {
          case IsZero(ev) => {
            (CellTree.toRoseTree(srcTree, ev) map ((cell => cell.value.item map (_.id)), (_ => None)), tgtExpr.item map (_.id))
          }
          case HasPred(ev) => {
            (CellTree.toRoseTree(srcTree, ev) map ((cell => cell.value.item map (_.id)), (leaf => leaf.value.item map (_.id))),
              tgtExpr.item map (_.id))
          }
        }
      }
    }

  class SimpleFrameworkCell(var item : Option[Expression])
      extends AbstractMutableCell with ExpressionFrameworkCell {

    def exprItem = item
  }
}
