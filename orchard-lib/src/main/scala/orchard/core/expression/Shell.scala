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

  // TODO : Equality

  def withFillingExpression(expr : Expression) : NCell[Expression] =
    framework.topCell.skeleton map (cell => {
      cell.item match {
        case None => expr
        case Some(e) => e
      }
    })

}
