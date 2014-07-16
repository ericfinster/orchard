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

  def render = {

    val viewBoxStr = "-1.0 10.0 " ++ 100.toString ++ " " ++ 100.toString

    <svg width={100.toString} height={100.toString} viewBox={viewBoxStr} xmlns="http://www.w3.org/2000/svg" version="1.1">
      <script type="text/javascript">
        window.onload = function() {{ 
          alert("You loaded an opetopic gallery!"); 
        }}
      </script>
    </svg>
  }

}
