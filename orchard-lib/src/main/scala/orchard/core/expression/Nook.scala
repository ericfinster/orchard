/**
  * Nook.scala - A simple wrapper class for nooks
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

import orchard.core.cell._

class Nook(val framework : ExpressionFramework) {

  assert(framework.topCell.isNook)

  val ncell : NCell[Option[Expression]] = framework.topCell.toNCell

  def map(f : Expression => Expression) : Nook = {
    val duplicate = framework.duplicate

    duplicate forAllCells (cell => {
      cell.item = cell.item map f
    })

    new Nook(duplicate)
  }

  def normalize : Nook = {
    val duplicate = framework.duplicate

    duplicate forAllFaces (cell => {
      cell.item foreach (e => {
        cell.item = Some(e.normalize)
        cell.promoteFaces
      })
    })

    new Nook(duplicate)

    // map (_.normalize)
  }

  def isThinBoundary : Boolean = 
    framework.topCell.isThinBoundary

  def withFiller(filler : Filler) : NCell[Expression] = {
    val frameworkCopy = framework.extract(framework.topCell)
    frameworkCopy.topCell.item = Some(filler)
    frameworkCopy.topCell.boundaryFace.item = Some(filler.Boundary)
    frameworkCopy.topCell.toNCell map (_.get)
  }

  def canEqual(other : Any) : Boolean = 
    other.isInstanceOf[Nook]

  override def equals(other : Any) : Boolean = 
    other match {
      case that : Nook =>
        (that canEqual this) && (that.ncell == this.ncell)
      case _ => false
    }

  override def hashCode : Int = 
    41 * (41 + ncell.hashCode)

}
