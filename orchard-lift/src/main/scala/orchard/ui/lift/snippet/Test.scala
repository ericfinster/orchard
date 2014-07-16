/**
  * Test.scala - Getting back up to speed with lift
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.lift.snippet

import orchard.core.svg._
import orchard.core.cell._
import orchard.core.complex._

object Test {

  val panel = new SimpleSVGPanel(new SimpleMutableComplex(Example.Psi), 2)

  def render = <p>This is added content</p>

}
