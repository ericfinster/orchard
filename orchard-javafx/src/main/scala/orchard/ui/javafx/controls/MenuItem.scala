/**
  * MenuItem.scala - Base class for items in a menu
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx.controls

import scalafx.scene.control.Label
import scalafx.scene.text.Text

import javafx.event.Event
import javafx.event.EventHandler
import javafx.scene.input.MouseEvent

import javafx.scene.{layout => jfxsl}

// Does this actually need to be a region at all????
class MenuItem extends jfxsl.Region {

  getStyleClass add "orch-menu-item"

  // It really seems this should be a label, but then
  // the initial guy doesn't show correctly ...
  val label = new Text
  getChildren add label

  def text : String = label.text()
  def text_=(s : String) = label.text = s

  var owner : Option[Menu] = None
  var action : Unit => Unit = (_ => ())

  def onAction : Unit => Unit = action
  def onAction_=(op : =>Unit) : Unit = action = (_ => op)

  override def computePrefWidth(height : Double) = label.getLayoutBounds.getWidth + getInsets.getLeft + getInsets.getRight
  override def computePrefHeight(width : Double) = label.getLayoutBounds.getHeight + getInsets.getTop + getInsets.getBottom

  override def layoutChildren = {
    super.layoutChildren
    label.relocate(getInsets.getLeft, getInsets.getTop)
  }

  setOnMouseClicked(new EventHandler[MouseEvent] {
    def handle(ev : MouseEvent) = { 
      owner foreach (o => o.close)
      action(())
    }
  })

}


