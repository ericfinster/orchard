/**
  * StaticGallery.scala - A simple gallery implementation using static panels
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.Includes._

import javafx.geometry.Pos
import javafx.scene.{layout => jfxsl}

abstract class StaticGallery[A] extends jfxsl.HBox with JavaFXGallery[A] {

  //============================================================================================
  // INITIALIZATION
  //

  getStyleClass add "orch-static-gallery"
  setAlignment(Pos.CENTER)
  setFillHeight(false)

  override type PanelType <: StaticPanel[A]

  myPanels onChange onMyPanelsChange

  def onMyPanelsChange : Unit = {
    getChildren setAll myPanels
  }

}
