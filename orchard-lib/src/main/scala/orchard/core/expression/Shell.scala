/**
  * Shell.scala - A simple wrapper class for shells
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

class Shell(val framework : ExpressionFramework) {

  assert(framework.topCell.isShell)

  // TODO : Equality

}
