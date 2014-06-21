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
          case KeyCode.P => if (ev.isControlDown) { if (ev.isShiftDown) onPasteToNewSheet else onPaste }
          case KeyCode.T => if (ev.isControlDown) onNewSheet
          // case KeyCode.O => if (ev.isControlDown) onOpenModule
          // case KeyCode.N => if (ev.isControlDown) onNewWorkspace
          // case KeyCode.S => if (ev.isControlDown) onSaveModule
          case KeyCode.F1 => onViewShell
          case KeyCode.F2 => onViewNook
          case KeyCode.F3 => onViewInterior
          case KeyCode.F4 => onViewNormalized
          case KeyCode.B => if (ev.isControlDown) onBind
          case KeyCode.N => if (ev.isControlDown) onNewModule else if (ev.isAltDown) onNewSubmodule
          case KeyCode.I => if (ev.isControlDown) onInstantiate
          case KeyCode.W => if (ev.isControlDown) onOpenWorkspace
          case KeyCode.X => if (ev.isControlDown) onImportCompleted
          case KeyCode.Z => if (ev.isControlDown) { if (ev.isShiftDown) onSetDebugFlag else onDebug }
          case KeyCode.SPACE => if (ev.isControlDown) onMarkExpression(ev.isShiftDown)
          case _ => ()
        }
      }
    })


  def onDebug : Unit =
    for {
      mod <- activeModule
      worksheet <- mod.activeWorksheet
      cell <- worksheet.selectionBase
      expr <- cell.expression
    } {
      consoleDebug("Expression: " ++ expr.toString)
    }

  def onSetDebugFlag : Unit = {
    import orchard.core.util._
    Util.debug = ! Util.debug
    consoleMessage("Debug is " ++ (if (Util.debug) "on" else "off"))
  }

  def onViewNormalized : Unit = 
    for {
      mod <- activeModule
      worksheet <- mod.activeWorksheet
      cell <- worksheet.selectionBase
      expr <- cell.expression
    } {
      mod.newSheet(expr.normalize)
    }

  def onViewNook : Unit = 
    for {
      mod <- activeModule
      worksheet <- mod.activeWorksheet
      cell <- worksheet.selectionBase
      expr <- cell.expression
    } {
      expr match {
        case f : Filler => 
          mod.newSheet(f.nook.framework)
        case _ => ()
      }
    }

  def onViewInterior : Unit = 
    for {
      mod <- activeModule
      worksheet <- mod.activeWorksheet
      cell <- worksheet.selectionBase
      expr <- cell.expression
    } {
      expr match {
        case b : Filler#BoundaryExpr => 
          mod.newSheet(b.interior)
        case _ => ()
      }
    }

  def onViewShell : Unit = 
    for {
      mod <- activeModule
      worksheet <- mod.activeWorksheet
      cell <- worksheet.selectionBase
      expr <- cell.expression
    } {
      expr match {
        case v : Variable => 
          mod.newSheet(v.shell.framework)
        case _ => ()
      }
    }

  def onExit : Unit = 
    scalafx.application.Platform.exit

  def onOpenWorkspace : Unit = 
    for {
      entry <- activeEntry
    } {
      activeModule = Some(entry.focusModule)
      displayEnvironment
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
          onOpenWorkspace
          subMod.newSheet
        })

      idDialog.setHeading("New Submodule")
      idDialog.run
    }

  // def onNewDefinition : Unit =
  //   for {
  //     mod <- activeModule
  //   } {
  //     val idDialog = 
  //       new SimpleIdentifierDialog(name => {
  //         val defn = new JavaFXDefinition(name, mod)
  //         mod.treeItem.children += defn.treeItem
  //         moduleView.selectionModel().select(defn.treeItem)
  //         defn.newSheet
  //         onOpenWorkspace
  //       })

  //     idDialog.setHeading("New Definition")
  //     idDialog.run
  //   }

  // def onDefine : Unit =
  //   for {
  //     wksp <- activeModule
  //     worksheet <- wksp.activeWorksheet
  //     if worksheet.selectionIsUnique
  //     selectedCell <- worksheet.selectionBase
  //   } {

  //     val fillerOpt = 
  //       selectedCell.expression match {
  //         case Some(f : Filler) => Some(f)
  //         case Some(b : Filler#BoundaryExpr) => Some(b.interior)
  //         case _ => {
  //           consoleError("Selected cell cannot be used as a definition.")
  //           None
  //         }
  //       }

  //     for { filler <- fillerOpt } {
  //       wksp match {
  //         case mod : JavaFXModule => {
  //           // Create a new definition with an empty parameter list ...

  //           val idDialog =
  //             new SimpleIdentifierDialog(name => {
  //               val defn = new JavaFXDefinition(name, mod)
  //               defn.filler = Some(filler)
  //               mod.treeItem.children += defn.treeItem
  //               moduleView.selectionModel().select(defn.treeItem)
  //             })

  //           idDialog.setHeading("New Definition")
  //           idDialog.run

  //         }
  //         case defn : JavaFXDefinition => {
  //           // Close the current definition with the selected cell
  //           defn.filler = Some(filler)

  //           // Now we need to get the tree cell to fix it's style.  Hmmm ....
  //           // Ahhh ... and we need to update the definition with some subcells
  //           // to show that the definition is ready ...  Maybe this will trigger
  //           // a redraw of the cell?

  //         }
  //       }
  //     }
  //   }

  def onInstantiate : Unit = 
    for {
      mod <- activeModule
      entry <- activeEntry
    } {
      entry match {
        case lift : JavaFXLift => {
          val instantiator = new JavaFXLiftInstantiator(mod, lift)
          mod.controlTabPane += instantiator.tab
          mod.controlTabPane.selectionModel().select(instantiator.tab)
        }
        case _ => ???
      }
    }

  def onBind : Unit =
    for {
      mod <- activeModule
      instntr <- mod.activeInstantiator
    } {
      instntr.bind
    }

  def onImportCompleted : Unit = 
    for {
      mod <- activeModule
    } {
      mod.importActiveInstantiation
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

  def onMarkExpression(markFiller : Boolean) : Unit = 
    for {
      mod <- activeModule
      worksheet <- mod.activeWorksheet
      selectedCell <- worksheet.selectionBase
      expr <- selectedCell.expression
    } {
      if (markFiller) {
        expr.normalizationResult match {
          case BoundaryResult => {
            consoleMessage("Okay to unfold.")
            mod.clipboardExpression = Some(Unfolding(expr, orchard.core.cell.Immediate))
          }
          case _ =>
            consoleError("Expression does not reduce to a boundary.")
        }
      } else {
        mod.clipboardExpression = Some(expr)
      }
    }

  def onPaste : Unit = 
    for {
      mod <- activeModule
      pasteExpr <- mod.clipboardExpression
    } {
      mod.pasteToSelection(pasteExpr)
    }

  def onPasteToNewSheet : Unit = 
    for {
      mod <- activeModule
      pasteExpr <- mod.clipboardExpression
    } {
      mod.newSheet(pasteExpr)
    }

}
