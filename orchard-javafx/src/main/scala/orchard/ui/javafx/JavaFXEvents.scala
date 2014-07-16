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
          // case KeyCode.LEFT => {
          //   for { 
          //     checker <- typeChecker
          //     wksp <- checker.activeWorkspace
          //     gallery <- wksp.activeGallery 
          //   } gallery.prev

          //   ev.consume
          // }
          // case KeyCode.RIGHT => {
          //   for { 
          //     checker <- typeChecker
          //     wksp <- checker.activeWorkspace
          //     gallery <- wksp.activeGallery 
          //   } gallery.next

          //   ev.consume
          // }
          // case KeyCode.E => if (ev.isControlDown) onExtrude
          // case KeyCode.D => if (ev.isControlDown) onDrop
          // case KeyCode.A => if (ev.isControlDown) onAssume(ev.isShiftDown)
          // case KeyCode.F => if (ev.isControlDown) onFill  
          // case KeyCode.P => if (ev.isControlDown) onPaste
          case KeyCode.T => if (ev.isControlDown) onNewWorksheet
          // case KeyCode.O => if (ev.isControlDown) onOpenModule
          // case KeyCode.S => if (ev.isControlDown) onSaveModule
          // case KeyCode.B => if (ev.isControlDown) onBind
          // case KeyCode.N => if (ev.isControlDown) onNewWorkspace
          // case KeyCode.N => if (ev.isControlDown) { if (ev.isShiftDown) onNewModule else onNewSubmodule }
          // case KeyCode.I => if (ev.isControlDown) onImportSubstitution
          // case KeyCode.V => if (ev.isControlDown) { if (ev.isShiftDown) onNewSubstInShell else onNewSubstitution }
          // case KeyCode.X => if (ev.isControlDown) onCloseWorkspace 
          // case KeyCode.L => if (ev.isControlDown) onAbstract
          // case KeyCode.W => if (ev.isControlDown) onCancelSubstitution
          // case KeyCode.R => if (ev.isControlDown) onRename
          // case KeyCode.U => if (ev.isControlDown) onUnify(ev.isShiftDown)
          // case KeyCode.G => if (ev.isControlDown) onGetEnvironmentCell
          // case KeyCode.V => if (ev.isControlDown) onView
          // case KeyCode.L => if (ev.isControlDown) onLoadExpr
          // case KeyCode.G => if (ev.isControlDown) onGlobCardinal
          // case KeyCode.X => if (ev.isControlDown) onExtra
          // case KeyCode.P => if (ev.isControlDown) onPrintScreen
          // case KeyCode.W => if (ev.isControlDown) onNewWorkspace
          // case KeyCode.M => if (ev.isControlDown) displayMessage("Message", "This is a message!")
          // case KeyCode.Z => if (ev.isControlDown) { debug = ! debug ; println("Debug is now: " ++ (if (debug) "on" else "off")) }
          case _ => ()
        }
      }
    })


  def onExit : Unit = 
    scalafx.application.Platform.exit

  def onNewWorksheet : Unit = createWorksheet

}
