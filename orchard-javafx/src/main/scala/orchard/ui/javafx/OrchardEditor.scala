/**
  * Editor.scala - Main module for JavaFX Orchard Editor
  * 
  * @author Eric Finster
  * @version 0.1
  */

package orchard.ui.javafx

import scalafx.Includes._

import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.stage.FileChooser

import scalafx.geometry._

import scalafx.scene.layout._
import scalafx.scene.control._

import scalafx.collections.ObservableBuffer

import javafx.event.Event
import javafx.event.EventHandler

import javafx.scene.input.KeyCode
import javafx.scene.input.KeyEvent
import javafx.scene.input.MouseEvent

import javafx.scene.{layout => jfxsl}
import javafx.scene.{control => jfxsc}

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.Map

import orchard.core.editor._

import orchard.ui.javafx.controls._

trait JavaFXEditor extends Editor {

  implicit def pm : PopupManager

  def newWorkspace(name : String, stabilityLevel : Option[Int], invertibilityLevel : Option[Int], unicityLevel : Option[Int]) : Unit
  def setPreviewGallery[A](gallery : SpinnerGallery[A]) : Unit
  def activeWorkspace : Option[JavaFXWorkspace]

}

object OrchardEditor extends PopupManager(new VBox) 
    with JavaFXEditor 
    with OrchardMenus 
    with OrchardDialogs { thisEditor =>

  implicit def pm = thisEditor

  val mainVBox = new VBox(root.delegate.asInstanceOf[jfxsl.VBox])

  val fileChooser = new FileChooser

  val workspaceListView = new ListView[JavaFXWorkspace]

  workspaceListView.selectionModel().selectedItem onChange {
    val item = workspaceListView.selectionModel().getSelectedItem
    
    if (item != null) {
      selectWorkspace(item)
    }
  }

  sealed trait DefinitionTreeItem
  // case class DefinitionItem(val defn : Definition) extends DefinitionTreeItem { override def toString = defn.name }
  // case class ExpressionItem[A](val expr : Expression[A]) extends DefinitionTreeItem { override def toString = expr.id }

  // class DefinitionTreeCell extends jfxsc.TreeCell[DefinitionTreeItem] {

  //   getStyleClass add "orch-list-cell"
  //   val cellStyleIndex = getStyleClass.length
  //   getStyleClass add "orch-list-null"

  //   def setCellStyleType(str : String) = {
  //     getStyleClass(cellStyleIndex) = str
  //   }

  //   override def updateItem(defnTreeItem : DefinitionTreeItem, empty : Boolean) = {
  //     super.updateItem(defnTreeItem, empty)

  //     if (! empty) {
  //       defnTreeItem match {
  //         case DefinitionItem(defn) => { setCellStyleType("orch-list-null") ; setText(defn.toString) }
  //         case ExpressionItem(expr) => { 
  //           setCellStyleType("orch-list-cell-" ++ expr.styleString)
  //           setText(expr.id)
  //         }
  //       }
  //     } else {
  //       setCellStyleType("orch-list-null")
  //       setText("")
  //     }
  //   }

  // }

  val definitionTreeRoot = new TreeItem[DefinitionTreeItem]
  val definitionTreeView = 
    new TreeView[DefinitionTreeItem] {
      root = definitionTreeRoot
      showRoot = false
      // cellFactory = (_ => new DefinitionTreeCell)
    }

  // definitionTreeView.selectionModel().selectedItem onChange {
  //   val item = definitionTreeView.selectionModel().getSelectedItem
    
  //   if (item != null) {
  //     item.value() match {
  //       case ExpressionItem(expr) => setPreview(expr)
  //       case _ => ()
  //     }
  //   }
  // }

  val workspacePane = new TitledPane {
    text = "Workspaces"
    content = workspaceListView
    collapsible = false
  }

  val workspaceAnchor = new AnchorPane {
    content = workspacePane
    styleClass += "orch-pane"
  }

  AnchorPane.setTopAnchor(workspacePane, 10)
  AnchorPane.setRightAnchor(workspacePane, 10)
  AnchorPane.setBottomAnchor(workspacePane, 10)
  AnchorPane.setLeftAnchor(workspacePane, 10)

  val definitionsPane = new TitledPane {
    text = "Local Definitions"
    content = definitionTreeView
    collapsible = false
  }

  val definitionsAnchor = new AnchorPane {
    content = definitionsPane
    styleClass += "orch-pane"
  }

  AnchorPane.setTopAnchor(definitionsPane, 10)
  AnchorPane.setRightAnchor(definitionsPane, 10)
  AnchorPane.setBottomAnchor(definitionsPane, 10)
  AnchorPane.setLeftAnchor(definitionsPane, 10)

  val noContextLabel = new Label("Empty Context")

  val contextPane = new TitledPane {
    text = "Context"
    content = noContextLabel
    collapsible = false
  }

  val contextAnchor = new AnchorPane {
    content = contextPane
    styleClass += "orch-pane"
  }

  AnchorPane.setTopAnchor(contextPane, 10)
  AnchorPane.setRightAnchor(contextPane, 10)
  AnchorPane.setBottomAnchor(contextPane, 10)
  AnchorPane.setLeftAnchor(contextPane, 10)

  val sheetPane = new StackPane {
    padding = Insets(10,10,10,10)
    styleClass += "orch-pane"
  }

  val previewPane = new StackPane {
    padding = Insets(0,10,0,10)
    styleClass += "orch-pane"
  }

  val leftVerticalSplit = new SplitPane {
    orientation = Orientation.VERTICAL
    items.addAll(workspaceAnchor, definitionsAnchor)
  }

  val middleVerticalSplit = new SplitPane {
    orientation = Orientation.VERTICAL
    items.addAll(sheetPane, previewPane)
    dividerPositions = 0.7f
  }

  val horizontalSplit = new SplitPane {
    orientation = Orientation.HORIZONTAL
    items.addAll(leftVerticalSplit, middleVerticalSplit, contextAnchor)
  }

  horizontalSplit.setDividerPositions(0.1f, 0.8f)

  VBox.setVgrow(horizontalSplit, Priority.ALWAYS)
  mainVBox.content.addAll(menuBar, horizontalSplit)

  //============================================================================================
  // EVENTS
  //

  addEventFilter(KeyEvent.KEY_PRESSED,
    new EventHandler[KeyEvent] {
      def handle(ev : KeyEvent) {
        ev.getCode match {
          case KeyCode.LEFT => {
            // if (ev.isControlDown) {
            //   val previewGallery = previewPane.content.head.asInstanceOf[SpinnerGallery[Any]]
            //   if (previewGallery != null)
            //     previewGallery.prev
            // } else
            //   for { gallery <- activeGallery } gallery.prev
          }
          case KeyCode.RIGHT => {
            // if (ev.isControlDown) {
            //   val previewGallery = previewPane.content.head.asInstanceOf[SpinnerGallery[Any]]
            //   if (previewGallery != null)
            //     previewGallery.next
            // } else 
            //   for { gallery <- activeGallery } gallery.next
          }
          case KeyCode.E => if (ev.isControlDown) onExtrude
          case KeyCode.D => if (ev.isControlDown) onDrop
          case KeyCode.A => if (ev.isControlDown) onAssume(ev.isShiftDown)
          case KeyCode.F => if (ev.isControlDown) onFill  
          case KeyCode.U => if (ev.isControlDown) onUseEnvironment
          case KeyCode.T => if (ev.isControlDown) onNewSheet
          case KeyCode.O => if (ev.isControlDown) onOpen
          case KeyCode.S => if (ev.isControlDown) onSave
  //         // case KeyCode.V => if (ev.isControlDown) onView
          case KeyCode.N => if (ev.isControlDown) onNewWorkspace
  //         // case KeyCode.L => if (ev.isControlDown) onLoadExpr
  //         // case KeyCode.G => if (ev.isControlDown) onGlobCardinal
  //         // case KeyCode.X => if (ev.isControlDown) onExtra
  //         // case KeyCode.P => if (ev.isControlDown) onPrintScreen
  //         // case KeyCode.W => if (ev.isControlDown) onWebView
  //         // case KeyCode.M => if (ev.isControlDown) displayMessage("Message", "This is a message!")
  //         // case KeyCode.Z => if (ev.isControlDown) { debug = ! debug ; println("Debug is now: " ++ (if (debug) "on" else "off")) }
          case _ => ()
        }
      }
    })

  def onExtrude : Unit = ??? // for { wksht <- activeSheet } { wksht.extrude }
  def onDrop : Unit = ??? // for { wksht <- activeSheet } { wksht.drop }

  def onAssume(thinHint : Boolean) : Unit = ??? // for { wksp <- activeWorkspace } { wksp.assumeAtSelection(thinHint) }
  def onFill : Unit = ??? // for { wksp <- activeWorkspace } { wksp.fillAtSelection }
  def onUseEnvironment : Unit = ??? // for { wksp <- activeWorkspace } { wksp.expressionToSelection }

  def onNewWorkspace = NewWorkspaceDialog.run
  def onNewSheet = ??? // for { wksp <- activeWorkspace } { wksp.newSheet }

  def onOpen = ???
  // {
  //   fileChooser.setTitle("Open")

  //   val file = fileChooser.showOpenDialog(getScene.getWindow)

  //   if (file != null) {
  //     loadDefinitions(file)
  //   }
  // }

  def onSave = ???
  // {
  //   fileChooser.setTitle("Save")

  //   val file = fileChooser.showSaveDialog(getScene.getWindow)

  //   if (file != null) {
  //     saveDefinitions(file)
  //   }
  // }

  def onExit : Unit = javafx.application.Platform.exit

  //============================================================================================
  // EDITOR HELPER ROUTINES
  //

  def withAssumptionInfo(thinHint : Boolean,
                         forceThin : Boolean,
                         handler : (String, Boolean) => Unit) : Unit = {
    val varDialog = new VariableDialog(handler)

    if (thinHint || forceThin) varDialog.thinCheckBox.selected = true
    if (forceThin) varDialog.thinCheckBox.disable = true

    varDialog.run
  }

  def withFillerIdentifiers(handler : (String, String) => Unit) : Unit = {
    val fillingDialog = new FillingDialog(handler)
    fillingDialog.run
  }

  def withFillerIdentifier(handler : String => Unit) : Unit = {
    val uniqueFillingDialog = new UniqueFillingDialog(handler)
    uniqueFillingDialog.run
  }

  var activeWorkspace : Option[JavaFXWorkspace] = None

  // def activeGallery : Option[JavaFXWorkspace#WorksheetGallery] = ???
    // for { wksp <- activeWorkspace ; gallery <- wksp.activeGallery } yield gallery

  // def activeSheet : Option[Workspace#Worksheet] = 
  //   for { 
  //     wksp <- activeWorkspace 
  //     sheet <- wksp.activeSheet
  //   } yield sheet

  // def activeDefinition : Option[Definition] = {
  //   val selectedDefnItem = definitionTreeView.getSelectionModel.selectedItem()

  //   if (selectedDefnItem != null) {
  //     Some(findParentDefinition(selectedDefnItem))
  //   } else None
  // }

  def newWorkspace(name : String, stabilityLevel : Option[Int], invertibilityLevel : Option[Int], unicityLevel : Option[Int]) = {
    val wksp = new JavaFXWorkspace(thisEditor, name, stabilityLevel, invertibilityLevel, unicityLevel)

    workspaceListView.items() += wksp
    workspaceListView.getSelectionModel.select(wksp)

    wksp.newSheet
  }

  def selectWorkspace(wksp : JavaFXWorkspace) = ()
  // {
  //   sheetPane.content = wksp.sheetTabPane
  //   contextPane.content = wksp.contextView

  //   if (wksp.isInstanceOf[JavaFXSubstitutionWorkspace]) {
  //     val substWksp = wksp.asInstanceOf[JavaFXSubstitutionWorkspace]
  //     substContextPane.content = substWksp.substContextView
  //   } else
  //     substContextPane.content = noSubstContextLabel

  //   activeWorkspace = Some(wksp)
  // }

  // def closeActiveWorkspace =
  //   for { wksp <- activeWorkspace } { closeWorkspace(wksp) }

  // def closeWorkspace(wksp : JavaFXWorkspace) = {
  //   val parent = wksp.treeItem.parent()

  //   if (parent != null) {
  //     parent.children.remove(wksp.treeItem)
  //   }

  //   contextPane.content = noContextLabel
  //   sheetPane.content.clear
  //   activeWorkspace = None
  // }

  def setPreviewGallery[A](gallery : SpinnerGallery[A]) = {
    // previewPane.content += gallery
    // gallery.refreshAll
  }

  // def clearDefinitions = {
  //   definitionTreeRoot.children.clear
  // }

  // def definitions : Seq[Definition] = {
  //   definitionTreeRoot.children map (child => {
  //     child.value().asInstanceOf[DefinitionItem].defn
  //   })
  // }

  // def findParentDefinition(treeItem : TreeItem[DefinitionTreeItem]) : Definition = {
  //   treeItem.value() match {
  //     case DefinitionItem(defn) => defn
  //     case _ => findParentDefinition(treeItem.parent())
  //   }
  // }

  // def addLocalDefinition(defn : Definition) = {
  //   val defnTreeItem = new TreeItem[DefinitionTreeItem] {
  //     value = DefinitionItem(defn)
  //     // children ++= defn.context map (expr => {
  //     //   new TreeItem[DefinitionTreeItem] {
  //     //     value = ExpressionItem(expr)
  //     //   }.delegate
  //     // })
  //   }

  //   definitionTreeRoot.children += defnTreeItem
  // }

  // def saveDefinitions(file : java.io.File) = {
  //   import XmlSerializable._

  //   val moduleXML = <module>{definitions map (defn => definitionSerializable.toXML(defn))}</module>
  //   xml.XML.save(file.getAbsolutePath, moduleXML)
  // }

  // def loadDefinitions(file : java.io.File) = {
  //   import XmlSerializable._

  //   clearDefinitions

  //   val elem = xml.XML.loadFile(file.getAbsolutePath)

  //   elem match {
  //     case <module>{defns @ _*}</module> => {
  //       trimText(defns) foreach (defXml => {
  //         val defn = definitionSerializable.fromXML(defXml)
  //         addLocalDefinition(defn)
  //       })        
  //     }
  //   }
  // }
}

object Editor extends JFXApp {

  val orchardScene = new Scene(OrchardEditor, 1600, 900)

  orchardScene.getStylesheets.add("orchard/ui/javafx/OrchardUI.css")

  stage = new JFXApp.PrimaryStage {
      title = "Orchard"
      scene = orchardScene
    }

}
