/**
  * UIExperiments.scala - A class where we can experiment with vaious UI features
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx.controls

import scala.collection.JavaConversions._
import scala.collection.mutable.HashMap

import scalafx.Includes._

import scalafx.scene.Node

import scalafx.scene.layout.HBox
import scalafx.scene.layout.VBox
import scalafx.scene.layout.Region
import scalafx.scene.layout.StackPane
import scalafx.scene.layout.AnchorPane

import scalafx.scene.shape.Rectangle
import scalafx.scene.paint.Color

import scalafx.scene.control.Button
import scalafx.scene.control.Label

import scalafx.scene.text.Text

import scalafx.collections.ObservableBuffer

import javafx.event.Event
import javafx.event.EventHandler
import javafx.scene.input.MouseEvent

import javafx.{scene => jfxs}
import javafx.scene.{control => jfxsc}
import javafx.scene.{layout => jfxsl}

object UIExperiments {

  val background = new StackPane 
  implicit val popupManager = new PopupManager(background)

  val file = new Menu {
    text = "File"
    items ++= List(new MenuItem { text = "Open" }, new MenuItem { text = "Save" }, new MenuItem { text = "Exit" })
  }

  val edit = new Menu {
    text = "Edit"
    items ++= List(new MenuItem { text = "Cut" }, new MenuItem { text = "Copy" }, new MenuItem { text = "Paste" })
  }

  val help = new Menu {
    text = "Help"
    items ++= List(new MenuItem { text = "Contents" }, new MenuItem { text = "About" })
  }


  val menuBar = new MenuBar {
    menus ++= List(file, edit, help)
  }

  // val fileMenu = new VerticalMenu {
  //   items ++= List(new MenuItem { text = "Open" }, new MenuItem { text = "Save" }, new MenuItem { text = "Exit" })
  // }

  background.content += menuBar

  // file.setOnMouseClicked(new EventHandler[MouseEvent]{
  //   def handle(ev : MouseEvent) = {
  //     println("You clicked file!")
  //     val fPos = file.localToScene(0, file.height())
  //     println("Computed file position as: " ++ fPos.toString)
  //     popupManager.showAt(fileMenu, fPos.x, fPos.y)
  //   }
  // })

}
