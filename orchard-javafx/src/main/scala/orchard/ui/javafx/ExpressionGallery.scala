/**
  * ExpressionGallery.scala - A Gallery for displaying expressions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import orchard.core._

class ExpressionGallery(seed : NCell[Expression]) extends JavaFXGallery[Expression] {

  //============================================================================================
  // INITIALIZATION
  //

  type PanelType = ExpressionPanel

  val complex = new ExpressionComplex(seed)

  def newPanel(i : Int) : ExpressionPanel = {
    val panel = new ExpressionPanel(complex, i)
    reactTo(panel) 
    panel 
  }

  initialize
}
