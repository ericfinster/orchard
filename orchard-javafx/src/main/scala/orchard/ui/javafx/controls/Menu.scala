/**
  * Menu.scala - A PopupRegion based menu
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx.controls

import scalafx.scene.layout.VBox

import scalafx.collections.ObservableBuffer

import javafx.scene.{layout => jfxsl}

class Menu(implicit pm : PopupManager) extends MenuItem {

  var bar : Option[MenuBar] = None

  val items : ObservableBuffer[MenuItem] = ObservableBuffer(Seq.empty)
  items onChange onItemsChange

  object MenuPopup extends PopupRegion {

    getStyleClass add "orch-menu"

    val vbox = new VBox { minWidth = 150 }
    getChildren add vbox

    override def layoutChildren = {
      super.layoutChildren
      vbox.relocate(getInsets.getLeft, getInsets.getTop)

      // Is this really how this is supposed to work????
      setPrefWidth(getInsets.getLeft + vbox.getLayoutBounds.getWidth + getInsets.getRight)
      setPrefHeight(getInsets.getTop + vbox.getLayoutBounds.getHeight + getInsets.getBottom)
    }
  }

  def close = bar foreach (b => b.hideMenu(this))

  def onItemsChange : Unit = {
    items foreach (item => {
      item.owner = Some(this)
      jfxsl.VBox.setVgrow(item, jfxsl.Priority.ALWAYS)
    })

    MenuPopup.vbox.children setAll items
  }

}
