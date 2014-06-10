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

  def isThinBoundary : Boolean = 
    framework.topCell.isThinBoundary

  def withFiller(filler : Filler) : NCell[Expression] = {
    val frameworkCopy = framework.extract(framework.topCell)
    frameworkCopy.topCell.item = Some(filler)
    frameworkCopy.topCell.boundaryFace.item = Some(filler.Boundary)
    frameworkCopy.topCell.toNCell map (_.get)
  }

  // Instead of doing this twice, implement the addressing system ...
  def withBoundary(bdry : Filler#BoundaryExpr) : NCell[Expression] = {
    val boundaryFramework = framework.extract(framework.topCell.boundaryFace)
    boundaryFramework.topCell.item = Some(bdry)
    boundaryFramework.topCell.toNCell map (_.get)
  }

  // TODO : Equality

}
