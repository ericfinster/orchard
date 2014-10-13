/**
  * StaticFrameworkGallery.scala - A static framework gallery
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import orchard.core.cell._
import orchard.core.expression._

class StaticFrameworkGallery(seed : NCell[Option[Expression]]) extends StaticGallery[Option[Expression]] {

  //============================================================================================
  // INITIALIZATION
  //

  type PanelType = StaticFrameworkPanel

  val complex = new SimpleFramework(seed)

  def newPanel(i : Int) : StaticFrameworkPanel = {
    val panel = new StaticFrameworkPanel(complex, i)
    reactTo(panel) 
    panel 
  }

  initialize
}
