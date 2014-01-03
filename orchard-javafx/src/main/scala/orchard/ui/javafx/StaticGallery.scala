/**
  * StaticGallery.scala - A simple gallery implementation using static panels
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.Includes._
import javafx.scene.{layout => jfxsl}
import orchard.core._

class StaticGallery[A](seed : NCell[A]) extends jfxsl.HBox with JavaFXGallery[A] {

  //============================================================================================
  // INITIALIZATION
  //

  type PanelType = StaticPanel[A]

  myPanels onChange onMyPanelsChange

  def onMyPanelsChange : Unit = {
    getChildren setAll myPanels
  }

  val complex = new SimpleMutableComplex[A](seed)

  def newPanel(i : Int) : StaticPanel[A] = {
    val panel = new StaticPanel(complex, i)
    reactTo(panel) 
    panel 
  }

  initialize
}
