/**
  * JavaFXEvents.scala - Event Routines for Orchard
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import javafx.event.Event
import javafx.event.EventHandler

import javafx.scene.input.KeyCode
import javafx.scene.input.KeyEvent
import javafx.scene.input.MouseEvent

trait JavaFXEvents { thisEditor : JavaFXEditor =>

  addEventFilter(KeyEvent.KEY_PRESSED,
    new EventHandler[KeyEvent] {
      def handle(ev : KeyEvent) {
        ev.getCode match {
          case KeyCode.R => if (ev.isControlDown) onRender
          case _ => ()
        }
      }
    })


  def onExit : Unit = 
    scalafx.application.Platform.exit

  def onRender : Unit = 
    renderExample

}
