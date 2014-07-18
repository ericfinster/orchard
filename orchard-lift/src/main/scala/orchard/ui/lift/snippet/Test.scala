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

import orchard.ui.lift._

object Test {

  val gallery = new LiftGallery(Example1.w)
  gallery.refreshAll

  def render = {
    <div class="jcarousel">
      <ul>
        { gallery.panels map (panel => { <li>{panel.toSVG}</li> }) }
      </ul>
      <a class="jcarousel-prev" href="#">Prev</a>
      <a class="jcarousel-next" href="#">Next</a>
    </div>
  }

}
