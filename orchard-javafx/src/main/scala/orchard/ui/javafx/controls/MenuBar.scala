/**
  * MenuBar.scala - A menu bar
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx.controls

import scalafx.Includes._

import scalafx.scene.layout.HBox

import scalafx.collections.ObservableBuffer

import javafx.event.Event
import javafx.event.EventHandler
import javafx.scene.input.MouseEvent

import javafx.scene.{layout => jfxsl}

class MenuBar(implicit pm : PopupManager) extends jfxsl.Region {

  getStyleClass add "orch-menu-bar"

  var openMenu : Option[Menu] = None

  val menus : ObservableBuffer[Menu] = ObservableBuffer(Seq.empty)
  menus onChange onMenusChange

  private val hbox = new HBox
  getChildren add hbox

  setMinHeight(jfxsl.Region.USE_PREF_SIZE)
  setMaxHeight(jfxsl.Region.USE_PREF_SIZE)

  def onMenusChange : Unit = {
    hbox.children setAll (menus map (m => {
      m.addEventHandler(MouseEvent.ANY,
        new EventHandler[MouseEvent] {
          def handle(ev : MouseEvent) = {
            ev.getEventType match {
              case MouseEvent.MOUSE_CLICKED => {
                openMenu match {
                  case None => showMenu(m)
                  case Some(menu) => {
                    hideMenu(menu)
                    if (menu != m) showMenu(m)
                  }
                }
              }
              case MouseEvent.MOUSE_ENTERED => {
                openMenu match {
                  case None => ()
                  case Some(menu) => {
                    if (menu != m) { hideMenu(menu) ; showMenu(m) }
                  }
                }
              }
              case _ => ()
            }
          }
        })

      m.bar = Some(this)
      m
    }))

  }

  def showMenu(menu : Menu) = {
    val pos = pm.sceneToLocal(menu.localToScene(0, menu.getHeight))
    pm.showAt(menu.MenuPopup, pos.x, pos.y + 3)
    menu.getStyleClass add "orch-menu-item-selected"
    openMenu = Some(menu)
  }

  def hideMenu(menu : Menu) = {
    pm.hide(menu.MenuPopup)
    menu.getStyleClass remove "orch-menu-item-selected"
    openMenu = None
  }

  override def layoutChildren = {
    super.layoutChildren
    hbox.relocate(getInsets.getLeft, getInsets.getTop)
  }

}
