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

import orchard.core.expression._

trait JavaFXEvents { thisEditor : JavaFXEditor =>

  addEventFilter(KeyEvent.KEY_PRESSED,
    new EventHandler[KeyEvent] {
      def handle(ev : KeyEvent) {
        ev.getCode match {
          case KeyCode.LEFT => {
            if (ev.isControlDown) {
              // val previewGallery = previewPane.content.head.asInstanceOf[SpinnerGallery[Any]]
              // if (previewGallery != null)
              //   previewGallery.prev
            } else
              for { 
                mod <- activeModule
                gallery <- mod.activeGallery 
              } gallery.prev

            ev.consume
          }
          case KeyCode.RIGHT => {
            if (ev.isControlDown) {
              // val previewGallery = previewPane.content.head.asInstanceOf[SpinnerGallery[Any]]
              // if (previewGallery != null)
              //   previewGallery.next
            } else 
              for { 
                mod <- activeModule
                gallery <- mod.activeGallery 
              } gallery.next

            ev.consume
          }
          case KeyCode.E => if (ev.isControlDown) onExtrude
          case KeyCode.D => if (ev.isControlDown) onDrop
          case KeyCode.A => if (ev.isControlDown) onAssume(ev.isShiftDown)
          case KeyCode.F => if (ev.isControlDown) onFill  
          case KeyCode.P => if (ev.isControlDown) onPaste
          case KeyCode.T => if (ev.isControlDown) onNewSheet
          // case KeyCode.O => if (ev.isControlDown) onOpenModule
          // case KeyCode.S => if (ev.isControlDown) onSaveModule
          // case KeyCode.B => if (ev.isControlDown) onBind
          // case KeyCode.N => if (ev.isControlDown) onNewWorkspace
          case KeyCode.N => if (ev.isControlDown) onNewModule
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
          // case KeyCode.W => if (ev.isControlDown) onWebView
          case KeyCode.M => if (ev.isControlDown) onNewSubmodule
          case KeyCode.Z => if (ev.isControlDown) onDebug
          case KeyCode.SPACE => if (ev.isControlDown) onMarkExpression
          case _ => ()
        }
      }
    })


  def onDebug : Unit =
    for {
      mod <- activeModule
      worksheet <- mod.activeWorksheet
      cell <- worksheet.selectionBase
    } {
      val addr = cell.address

      consoleMessage("Cell has address: " ++ addr.toString)

      val theCell = worksheet.seek(addr).get

      consoleMessage("Cell at that address is: " ++ theCell.expression.toString)
    }

  def onExit : Unit = 
    scalafx.application.Platform.exit

  def onNewModule : Unit = 
    NewModuleDialog.run

  def onNewSubmodule : Unit =
    for {
      mod <- activeModule
    } {
      def idHandler(name : String) = mod.appendSubmodule(name)
      val idDialog = new SimpleIdentifierDialog(idHandler)
      idDialog.setHeading("New Submodule")
      idDialog.run
    }

  def onNewSheet : Unit =
    for {
      mod <- activeModule
    } {
      mod.newSheet
    }

  def onExtrude : Unit =
    for {
      mod <- activeModule
      worksheet <- mod.activeWorksheet
    } {
      worksheet.extrude
    }

  def onDrop : Unit =
    for {
      mod <- activeModule
      worksheet <- mod.activeWorksheet
    } {
      worksheet.drop
    }

  def onAssume(thinHint : Boolean) : Unit = 
    for {
      mod <- activeModule
    } {
      mod.assumeAtSelection(thinHint)
    }

  def onFill : Unit =
    for {
      mod <- activeModule
    } {
      mod.fillAtSelection
    }

  def onMarkExpression : Unit = 
    for {
      mod <- activeModule
      worksheet <- mod.activeWorksheet
      selectedCell <- worksheet.selectionBase
    } {
      mod.clipboardExpression = selectedCell.expression
    }

  def onPaste : Unit = 
    for {
      mod <- activeModule
      worksheet <- mod.activeWorksheet
    } {
      
    }
}
