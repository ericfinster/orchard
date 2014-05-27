/**
  * OrchardEditor.scala - Orchard Editor Routines
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.Includes._

import scalafx.scene.Scene
import scalafx.scene.layout.VBox
import scalafx.scene.layout.Priority
import scalafx.application.JFXApp

import javafx.scene.{layout => jfxsl}

import orchard.ui.javafx.controls.PopupManager

object OrchardEditor extends PopupManager(new VBox)
    with JavaFXEditor 
    with JavaFXUI
    with JavaFXEvents
    with JavaFXMenus
    with JavaFXDialogs { 

  implicit def pm = this

  val mainVBox = new VBox(root.delegate.asInstanceOf[jfxsl.VBox])
  VBox.setVgrow(ui, Priority.ALWAYS)
  mainVBox.content.addAll(menuBar, ui)

}

object OrchardApp extends JFXApp {

  val orchardScene = new Scene(OrchardEditor, 1600, 900)

  orchardScene.getStylesheets.add("orchard/ui/javafx/OrchardUI.css")

  stage = new JFXApp.PrimaryStage {
      title = "Orchard"
      scene = orchardScene
    }

}
