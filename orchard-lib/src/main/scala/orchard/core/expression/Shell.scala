/**
  * Shell.scala - A simple wrapper class for shells
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

import orchard.core.cell._

class Shell(val framework : ExpressionFramework) {

  assert(framework.topCell.isShell)

  val ncell : NCell[Option[Expression]] = framework.topCell.toNCell

  def map(f : Expression => Expression) : Shell = {
    val duplicate = framework.duplicate

    duplicate forAllCells (cell => {
      cell.item = cell.item map f
    })

    new Shell(duplicate)
  }

  def withFillingExpression(expr : Expression) : NCell[Expression] =
    framework.topCell.skeleton map (cell => {
      cell.item match {
        case None => expr
        case Some(e) => e
      }
    })

  def canEqual(other : Any) : Boolean = 
    other.isInstanceOf[Shell]

  override def equals(other : Any) : Boolean = 
    other match {
      case that : Shell =>
        (that canEqual this) && (that.ncell == this.ncell)
      case _ => false
    }

  override def hashCode : Int = 
    41 * (41 + ncell.hashCode)

}


