/**
  * Nook.scala - A simple wrapper class for nooks
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

class Nook(val framework : ExpressionFramework) {

  assert(framework.topCell.isNook)

  def isThinBoundary : Boolean = 
    framework.topCell.isThinBoundary

  // TODO : Equality

}
