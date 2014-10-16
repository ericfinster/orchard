/**
  * OrchardEditor.scala - Orchard Editor Routines
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.Includes._

import scalafx.scene.Node
import scalafx.scene.Scene
import scalafx.scene.layout.VBox
import scalafx.scene.layout.Priority
import scalafx.application.JFXApp

import javafx.scene.{layout => jfxsl}

import controls._

object OrchardEditor extends JavaFXEditor {

  val mainVBox = new VBox(root.delegate.asInstanceOf[jfxsl.VBox])

  VBox.setVgrow(ui, Priority.Always)
  mainVBox.content.addAll(menuBar, ui)

  consoleWrite("Welcome to Orchard!")

}

object OrchardApp extends JFXApp {

  val orchardScene = new Scene(OrchardEditor, 1600, 900)

  orchardScene.getStylesheets.add("orchard/ui/javafx/OrchardUI.css")

  stage = new JFXApp.PrimaryStage {
      title = "Orchard"
      scene = orchardScene
    }

}
