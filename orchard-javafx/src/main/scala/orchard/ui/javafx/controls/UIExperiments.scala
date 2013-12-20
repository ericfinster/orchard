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

class MenuBar(implicit pm : PopupManager) extends jfxsl.Region {

  var openMenu : Option[Menu] = None

  val menus : ObservableBuffer[Menu] = ObservableBuffer(Seq.empty)
  menus onChange onMenusChange

  private val hbox = new HBox
  hbox.setStyle("-fx-background-color: gainsboro;")
  getChildren add hbox

  setMaxHeight(jfxsl.Region.USE_PREF_SIZE)

  def onMenusChange : Unit = {
    hbox.children setAll (menus map (m => {
      val label = m.label

      m.label.addEventHandler(MouseEvent.ANY,
        new EventHandler[MouseEvent] {
          def handle(ev : MouseEvent) = {
            ev.getEventType match {
              case MouseEvent.MOUSE_CLICKED => {
                openMenu match {
                  case None => showMenu(label.owner.asInstanceOf[Menu])
                  case Some(menu) => {
                    hideMenu(menu)
                    if (menu != label.owner) showMenu(label.owner.asInstanceOf[Menu])
                  }
                }
              }
              case MouseEvent.MOUSE_ENTERED => {
                openMenu match {
                  case None => ()
                  case Some(menu) => {
                    if (menu != label.owner) { hideMenu(menu) ; showMenu(label.owner.asInstanceOf[Menu]) }
                  }
                }
              }
              case _ => ()
            }
          }
        })

      m.label.delegate
    }))

  }

  def showMenu(menu : Menu) = {
    val pos = pm.sceneToLocal(menu.label.localToScene(0, menu.label.height()))
    pm.showAt(menu, pos.x, pos.y)
    openMenu = Some(menu)
  }

  def hideMenu(menu : Menu) = {
    pm.hide(menu)
    openMenu = None
  }

  override def computePrefWidth(height : Double) = hbox.getLayoutBounds.getWidth + getInsets.getLeft + getInsets.getRight
  override def computePrefHeight(width : Double) = hbox.getLayoutBounds.getHeight + getInsets.getTop + getInsets.getBottom

  override def layoutChildren = {
    val insets = getInsets
    val left = insets.getLeft
    val right = insets.getRight
    val top = insets.getTop

    hbox.autosize
    hbox.resizeRelocate(left, top, getWidth - left - right, hbox.getHeight)
  }

}

class MenuLabel(val owner : MenuItem) extends jfxsl.Region {

  getStyleClass add "orch-menu-label"

  val lbl = new Label
  getChildren add lbl

  def text : String = lbl.text()
  def text_=(s : String) = lbl.text = s

  override def layoutChildren = {
    super.layoutChildren
    lbl.relocate(getInsets.left, getInsets.top)
  }
}

// Does this actually need to be a region at all????
class MenuItem extends PopupRegion {

  val label = new MenuLabel(this)

  setMaxSize(jfxsl.Region.USE_PREF_SIZE, jfxsl.Region.USE_PREF_SIZE)

  def text : String = label.text
  def text_=(s : String) = label.text = s

  override def computePrefWidth(height : Double) = label.getLayoutBounds.getWidth + getInsets.getLeft + getInsets.getRight
  override def computePrefHeight(width : Double) = label.getLayoutBounds.getHeight + getInsets.getTop + getInsets.getBottom

  override def layoutChildren = {
    super.layoutChildren
    label.relocate(getInsets.left, getInsets.top)
  }

}

class Menu(implicit pm : PopupManager) extends MenuItem {

  val items : ObservableBuffer[MenuItem] = ObservableBuffer(Seq.empty)
  items onChange onItemsChange

  private val vbox = new VBox
  getStyleClass add "orch-menu"
  //vbox.setStyle("-fx-background-color: gainsboro; -fx-border-color: grey; -fx-border-radius: 1pt;")
  getChildren add vbox

  setMaxSize(jfxsl.Region.USE_PREF_SIZE, jfxsl.Region.USE_PREF_SIZE)

  def onItemsChange : Unit = vbox.children setAll (items map (item => item.label.delegate))

  override def computePrefWidth(height : Double) = vbox.getLayoutBounds.getWidth + getInsets.getLeft + getInsets.getRight
  override def computePrefHeight(width : Double) = vbox.getLayoutBounds.getWidth + getInsets.getTop + getInsets.getBottom

  override def layoutChildren = {
    vbox.autosize
    vbox.relocate(getInsets.left, getInsets.top)
  }

}

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
