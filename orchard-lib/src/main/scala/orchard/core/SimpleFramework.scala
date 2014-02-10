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

class SimpleFramework(seed : NCell[Option[Expression]]) 
    extends AbstractMutableComplex[Option[Expression]](seed) with ExpressionFramework[Option[Expression]] {

  type CellType = SimpleFrameworkCell

  def newCell(item : Option[Expression]) = new SimpleFrameworkCell(item)

  // If this is an exposed nook, return a minimal amount of information to
  // reconstruct it in a given environment
  def getShallowNook : ShallowNook = // (RoseTree[Option[String], Option[String]], Option[String]) =
    topCell.skeleton.cell match {
      case Composite(_, srcTree, tgtExpr, _) => {
        srcTree.dimension match {
          case IsZero(ev) => {
            ShallowNook(CellTree.toRoseTree(srcTree, ev) map ((cell => cell.value.item map (_.id)), (_ => None)), tgtExpr.item map (_.id))
          }
          case HasPred(ev) => {
            ShallowNook(CellTree.toRoseTree(srcTree, ev) map ((cell => cell.value.item map (_.id)), (leaf => leaf.value.item map (_.id))),
              tgtExpr.item map (_.id))
          }
        }
      }
    }

  def dependencies : Map[String, NCell[Expression]] = {
    val deps = HashMap.empty[String, NCell[Expression]]
    collectDependencies(deps)
    deps
  }
   
  def freeVariables : Map[String, NCell[Expression]] = {
    dependencies filter (pr => {
      val (id, expr) = pr
      expr.value match {
        case Variable(_, _) => true
        case _ => false
      }
    })
  }

  def collectDependencies(deps : Map[String, NCell[Expression]]) : Unit = {
    forAllCells(cell => {
      cell.item match {
        case None => ()
        case Some(expr) => {
          expr match {
            case Filler(id, nook) => new SimpleFramework(nook).collectDependencies(deps)
            case FillerFace(id, nook, isThin) => new SimpleFramework(nook).collectDependencies(deps)
            case _ => ()
          }

          deps(expr.id) = cell.getSimpleFramework.toCell map (_.get)
        }
      }
    })
  }

  class SimpleFrameworkCell(var item : Option[Expression])
      extends AbstractMutableCell with ExpressionFrameworkCell {
    def exprItem = item
  }
}
