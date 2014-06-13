/**
  * JavaFXEvents.scala - Event Routines for Orchard
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.Includes._

import javafx.event.Event
import javafx.event.EventHandler

import javafx.scene.input.KeyCode
import javafx.scene.input.KeyEvent
import javafx.scene.input.MouseEvent

import orchard.core.expression._

import JavaFXModuleSystem._

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
                wksp <- activeWorkspace
                gallery <- wksp.activeGallery 
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
                wksp <- activeWorkspace
                gallery <- wksp.activeGallery 
              } gallery.next

            ev.consume
          }
          case KeyCode.E => if (ev.isControlDown) onExtrude
          case KeyCode.D => if (ev.isControlDown) onDrop else if (ev.isAltDown) onNewDefinition
          case KeyCode.A => if (ev.isControlDown) onAssume(ev.isShiftDown)
          case KeyCode.F => if (ev.isControlDown) onFill  
          case KeyCode.P => if (ev.isControlDown) onPaste
          case KeyCode.T => if (ev.isControlDown) onNewSheet
          // case KeyCode.O => if (ev.isControlDown) onOpenModule
          // case KeyCode.S => if (ev.isControlDown) onSaveModule
          // case KeyCode.N => if (ev.isControlDown) onNewWorkspace
          case KeyCode.B => if (ev.isControlDown) onBind
          case KeyCode.N => if (ev.isControlDown) onNewModule else if (ev.isAltDown) onNewSubmodule
          case KeyCode.I => if (ev.isControlDown) onInstantiate
          case KeyCode.X => if (ev.isControlDown) onImportCompleted
          // case KeyCode.V => if (ev.isControlDown) { if (ev.isShiftDown) onNewSubstInShell else onNewSubstitution }
          // case KeyCode.X => if (ev.isControlDown) onCloseWorkspace 
          // case KeyCode.W => if (ev.isControlDown) onCancelSubstitution
          // case KeyCode.R => if (ev.isControlDown) onRename
          // case KeyCode.U => if (ev.isControlDown) onUnify(ev.isShiftDown)
          // case KeyCode.G => if (ev.isControlDown) onGetEnvironmentCell
          case KeyCode.V => if (ev.isControlDown) onViewNormalized else if (ev.isAltDown) onViewNook else if (ev.isShiftDown) onViewInterior
          case KeyCode.L => if (ev.isControlDown) onDefine
          // case KeyCode.G => if (ev.isControlDown) onGlobCardinal
          // case KeyCode.X => if (ev.isControlDown) onExtra
          // case KeyCode.P => if (ev.isControlDown) onPrintScreen
          case KeyCode.W => if (ev.isControlDown) onOpenWorkspace
          case KeyCode.Z => if (ev.isControlDown) onDebug
          case KeyCode.SPACE => if (ev.isControlDown) onMarkExpression
          case _ => ()
        }
      }
    })


  def onDebug : Unit =
    for {
      wksp <- activeWorkspace
      worksheet <- wksp.activeWorksheet
      cell <- worksheet.selectionBase
      expr <- cell.expression
    } {
      consoleDebug("Expression: " ++ expr.toString)
      consoleDebug("Normalized expression:" ++ expr.normalize.toString)
    }

  def onViewNormalized : Unit = 
    for {
      wksp <- activeWorkspace
      worksheet <- wksp.activeWorksheet
      cell <- worksheet.selectionBase
      expr <- cell.expression
    } {
      wksp.newSheet(expr.normalize)
    }

  def onViewNook : Unit = 
    for {
      wksp <- activeWorkspace
      worksheet <- wksp.activeWorksheet
      cell <- worksheet.selectionBase
      expr <- cell.expression
    } {
      expr match {
        case f : Filler => 
          wksp.newSheet(f.nook.framework)
        case _ => ()
      }
    }

  def onViewInterior : Unit = 
    for {
      wksp <- activeWorkspace
      worksheet <- wksp.activeWorksheet
      cell <- worksheet.selectionBase
      expr <- cell.expression
    } {
      expr match {
        case b : Filler#BoundaryExpr => 
          wksp.newSheet(b.interior)
        case _ => ()
      }
    }

  def onExit : Unit = 
    scalafx.application.Platform.exit

  def onOpenWorkspace : Unit = 
    for {
      entry <- activeEntry
    } {
      displayParameters(entry)
      activeWorkspace = entry.focusWorkspace
      activeModule = Some(entry.focusModule)
    }

  def onNewModule : Unit = 
    NewModuleDialog.run

  def onNewSubmodule : Unit =
    for {
      mod <- activeModule
    } {
      val idDialog = 
        new SimpleIdentifierDialog(name => {
          val subMod = 
            new JavaFXModule(
              name,
              Some(mod), 
              mod.stabilityLevel, 
              mod.invertibilityLevel, 
              mod.unicityLevel
            )

          mod.treeItem.children += subMod.treeItem
          moduleView.selectionModel().select(subMod.treeItem)
        })

      idDialog.setHeading("New Submodule")
      idDialog.run
    }

  def onNewDefinition : Unit =
    for {
      mod <- activeModule
    } {
      val idDialog = 
        new SimpleIdentifierDialog(name => {
          val defn = new JavaFXDefinition(name, mod)
          mod.treeItem.children += defn.treeItem
          moduleView.selectionModel().select(defn.treeItem)
          defn.newSheet
          onOpenWorkspace
        })

      idDialog.setHeading("New Definition")
      idDialog.run
    }

  def onDefine : Unit =
    for {
      wksp <- activeWorkspace
      worksheet <- wksp.activeWorksheet
      if worksheet.selectionIsUnique
      selectedCell <- worksheet.selectionBase
    } {

      val fillerOpt = 
        selectedCell.expression match {
          case Some(f : Filler) => Some(f)
          case Some(b : Filler#BoundaryExpr) => Some(b.interior)
          case _ => {
            consoleError("Selected cell cannot be used as a definition.")
            None
          }
        }

      for { filler <- fillerOpt } {
        wksp match {
          case mod : JavaFXModule => {
            // Create a new definition with an empty parameter list ...

            val idDialog =
              new SimpleIdentifierDialog(name => {
                val defn = new JavaFXDefinition(name, mod)
                defn.filler = Some(filler)
                mod.treeItem.children += defn.treeItem
                moduleView.selectionModel().select(defn.treeItem)
              })

            idDialog.setHeading("New Definition")
            idDialog.run

          }
          case defn : JavaFXDefinition => {
            // Close the current definition with the selected cell
            defn.filler = Some(filler)

            // Now we need to get the tree cell to fix it's style.  Hmmm ....
            // Ahhh ... and we need to update the definition with some subcells
            // to show that the definition is ready ...  Maybe this will trigger
            // a redraw of the cell?

          }
        }
      }
    }

  def onInstantiate : Unit = 
    for {
      wksp <- activeWorkspace
      entry <- activeEntry
    } {
      entry.focusWorkspace match {
        case defn : JavaFXDefinition => {
          val instantiator = new JavaFXDefinitionInstantiator(wksp, defn)
          wksp.controlTabPane += instantiator.tab
          wksp.controlTabPane.selectionModel().select(instantiator.tab)
        }
        case _ => ???
      }
    }

  def onBind : Unit =
    for {
      wksp <- activeWorkspace
      instntr <- wksp.activeInstantiator
    } {
      instntr.bind
    }

  def onImportCompleted : Unit = 
    for {
      wksp <- activeWorkspace
      instntr <- wksp.activeInstantiator
    } {
      if (instntr.isComplete) {
        wksp.newSheet(instntr.completedExpression)
      } else {
        consoleError("There are unbound variables.")
      }
    }

  def onNewSheet : Unit =
    for {
      wksp <- activeWorkspace
    } {
      wksp.newSheet
    }

  def onExtrude : Unit =
    for {
      wksp <- activeWorkspace
      worksheet <- wksp.activeWorksheet
    } {
      worksheet.extrude
    }

  def onDrop : Unit =
    for {
      wksp <- activeWorkspace
      worksheet <- wksp.activeWorksheet
    } {
      worksheet.drop
    }

  def onAssume(thinHint : Boolean) : Unit = 
    for {
      wksp <- activeWorkspace
    } {
      wksp.assumeAtSelection(thinHint)
    }

  def onFill : Unit =
    for {
      wksp <- activeWorkspace
    } {
      wksp.fillAtSelection
    }

  def onMarkExpression : Unit = 
    for {
      wksp <- activeWorkspace
      worksheet <- wksp.activeWorksheet
      selectedCell <- worksheet.selectionBase
    } {
      wksp.clipboardExpression = selectedCell.expression
    }

  def onPaste : Unit = 
    for {
      wksp <- activeWorkspace
      pasteExpr <- wksp.clipboardExpression
    } {
      wksp.pasteToSelection(pasteExpr)
    }

}
