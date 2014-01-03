/**
  * FrameworkGallery.scala - A Gallery for displaying frameworks
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import orchard.core._

class FrameworkGallery(seed : NCell[Option[Expression]]) extends SpinnerGallery[Option[Expression]] {

  //============================================================================================
  // INITIALIZATION
  //

  type PanelType = FrameworkPanel

  val complex = new SimpleFramework(seed)

  def newPanel(i : Int) : FrameworkPanel = {
    val panel = new FrameworkPanel(complex, i)
    reactTo(panel) 
    panel 
  }

  initialize
}
