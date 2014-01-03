/**
  * SpinnerGallery.scala - A Gallery based on a spinner
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.Includes._

import orchard.core._
import orchard.ui.javafx.controls.Spinner

abstract class SpinnerGallery[A] extends Spinner with JavaFXGallery[A] {

  override type PanelType <: ZoomPanel[A]

  getStyleClass add "orch-spinner-gallery"

  myPanels onChange onMyPanelsChange

  def onMyPanelsChange : Unit = {
    hbox.getChildren setAll myPanels
  }

}
