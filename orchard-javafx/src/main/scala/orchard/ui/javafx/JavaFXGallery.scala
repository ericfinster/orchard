/**
  * JavaFXGallery.scala - An abstract JavaFXGallery
  * 
  * @author Eric Finster
  * @version 0.1
  */

package orchard.ui.javafx

import orchard.core._

abstract class JavaFXGallery[A] extends Spinner with Gallery[A] {

  override type PanelType <: JavaFXPanel[A]

  //============================================================================================
  // UI INITIALIZATION
  //

  getStyleClass().add("javafx-gallery")
  
}
