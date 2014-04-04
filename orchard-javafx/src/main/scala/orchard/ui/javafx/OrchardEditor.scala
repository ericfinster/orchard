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

import orchard.core._
import orchard.ui.javafx.controls._

import Environment._

trait JavaFXEditor extends Editor {

  implicit def pm : PopupManager

  def newDefinition(name : String, stabilityLevel : Option[Int], invertibilityLevel : Option[Int], unicityLevel : Option[Int]) : Unit
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

  val navigationTreeRoot = new TreeItem[JavaFXWorkspace]
  val navigationTreeView = 
    new TreeView[JavaFXWorkspace] {
      root = navigationTreeRoot
      showRoot = false
    }

  navigationTreeView.selectionModel().selectedItem onChange {
    val item = navigationTreeView.selectionModel().getSelectedItem
    
    if (item != null) {
      selectWorkspace(item.value())
    }
  }

  sealed trait DefinitionTreeItem
  case class DefinitionItem(val defn : Definition) extends DefinitionTreeItem { override def toString = defn.name }
  case class ExpressionItem[A](val expr : Expression[A]) extends DefinitionTreeItem { override def toString = expr.id }

  class DefinitionTreeCell extends jfxsc.TreeCell[DefinitionTreeItem] {

    getStyleClass add "orch-list-cell"
    val cellStyleIndex = getStyleClass.length
    getStyleClass add "orch-list-null"

    def setCellStyleType(str : String) = {
      getStyleClass(cellStyleIndex) = str
    }

    override def updateItem(defnTreeItem : DefinitionTreeItem, empty : Boolean) = {
      super.updateItem(defnTreeItem, empty)

      if (! empty) {
        defnTreeItem match {
          case DefinitionItem(defn) => { setCellStyleType("orch-list-null") ; setText(defn.toString) }
          case ExpressionItem(expr) => { 
            setCellStyleType("orch-list-cell-" ++ expr.styleString)
            setText(expr.id)
          }
        }
      } else {
        setCellStyleType("orch-list-null")
        setText("")
      }
    }

  }

  val definitionTreeRoot = new TreeItem[DefinitionTreeItem]
  val definitionTreeView = 
    new TreeView[DefinitionTreeItem] {
      root = definitionTreeRoot
      showRoot = false
      cellFactory = (_ => new DefinitionTreeCell)
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

  val navigationPane = new TitledPane {
    text = "Navigation"
    content = navigationTreeView
    collapsible = false
  }

  val navigationAnchor = new AnchorPane {
    content = navigationPane
    styleClass += "orch-pane"
  }

  AnchorPane.setTopAnchor(navigationPane, 10)
  AnchorPane.setRightAnchor(navigationPane, 10)
  AnchorPane.setBottomAnchor(navigationPane, 10)
  AnchorPane.setLeftAnchor(navigationPane, 10)

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
  val noSubstContextLabel = new Label("No Substitution Active")

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

  val substContextPane = new TitledPane {
    text = "Substitution Context"
    content = noSubstContextLabel
    collapsible = false
  }

  val substContextAnchor = new AnchorPane {
    content = substContextPane
    styleClass += "orch-pane"
  }

  AnchorPane.setTopAnchor(substContextPane, 10)
  AnchorPane.setRightAnchor(substContextPane, 10)
  AnchorPane.setBottomAnchor(substContextPane, 10)
  AnchorPane.setLeftAnchor(substContextPane, 10)

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
    items.addAll(navigationAnchor, definitionsAnchor)
  }

  val middleVerticalSplit = new SplitPane {
    orientation = Orientation.VERTICAL
    items.addAll(sheetPane, previewPane)
    dividerPositions = 0.7f
  }

  val rightVerticalSplit = new SplitPane {
    orientation = Orientation.VERTICAL
    items.addAll(contextAnchor, substContextAnchor)
  }

  val horizontalSplit = new SplitPane {
    orientation = Orientation.HORIZONTAL
    items.addAll(leftVerticalSplit, middleVerticalSplit, rightVerticalSplit)
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
            if (ev.isControlDown) {
              val previewGallery = previewPane.content.head.asInstanceOf[SpinnerGallery[Any]]
              if (previewGallery != null)
                previewGallery.prev
            } else
              for { gallery <- activeGallery } gallery.prev
          }
          case KeyCode.RIGHT => {
            if (ev.isControlDown) {
              val previewGallery = previewPane.content.head.asInstanceOf[SpinnerGallery[Any]]
              if (previewGallery != null)
                previewGallery.next
            } else 
              for { gallery <- activeGallery } gallery.next
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
          case KeyCode.N => if (ev.isControlDown) onNewDefinition
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

  def onExtrude : Unit = for { wksht <- activeSheet } { wksht.extrude }
  def onDrop : Unit = for { wksht <- activeSheet } { wksht.drop }

  def onAssume(thinHint : Boolean) : Unit = for { wksp <- activeWorkspace } { wksp.assumeAtSelection(thinHint) }
  def onFill : Unit = for { wksp <- activeWorkspace } { wksp.fillAtSelection }
  def onUseEnvironment : Unit = for { wksp <- activeWorkspace } { wksp.expressionToSelection }

  def onNewDefinition = NewDefinitionDialog.run
  def onNewSheet = for { wksp <- activeWorkspace } { wksp.newSheet }

  def onCompleteDefinition =
    for {
      wksp <- activeWorkspace
    } {
      if (wksp.isInstanceOf[JavaFXDefinitionWorkspace]) {
        val defnWksp = wksp.asInstanceOf[JavaFXDefinitionWorkspace]
        
        for {
          defn <- defnWksp.completeDefinition
        } {
          addLocalDefinition(defn)
          closeActiveWorkspace
        }
      }
    }

  def onDeleteDefinition = {
    val defnItem = definitionTreeView.getSelectionModel.selectedItem()

    if (defnItem != null) {
      definitionTreeRoot.children -= defnItem
    }
  }

  def onApply =
    for {
      wksp <- activeWorkspace
      defn <- activeDefinition
      substWksp <- wksp.apply(defn)
    } {
      navigationTreeView.getSelectionModel.select(substWksp.treeItem)
    }

  def onApplyInShell =
    for { 
      wksp <- activeWorkspace
      gallery <- wksp.activeGallery
      cell <- gallery.complex.selectionBase
      defn <- activeDefinition
    } {
      if (gallery.complex.selectionIsUnique && cell.hasCompleteShell) {

        val shell = Object(Seq.empty)
        // {
        //   val frmwk = gallery.complex.extract(cell)
        //   frmwk.topCell.item = Neutral(Seq.empty)
        //   frmwk.topCell.neutralNCell
        // }

        for {
          substWksp <- wksp.applyInShell(shell, defn)
        } {
          navigationTreeView.getSelectionModel.select(substWksp.treeItem)
        }
      } else {
        println("Cannot spawn application here.")
      }
    }

  def onSatisfyGoal = ???
    // for {
    //   wksp <- activeWorkspace
    //   gallery <- wksp.activeGallery
    //   selectedCell <- gallery.complex.selectionBase
    // } {
    //   if (gallery.complex.selectionIsUnique) {
    //     if (wksp.isInstanceOf[JavaFXSubstitutionWorkspace]) {
    //       if (selectedCell.isComplete) {
    //         val substWksp = wksp.asInstanceOf[JavaFXSubstitutionWorkspace]
    //         val selectedGoal = substWksp.goalsListView.getSelectionModel.selectedItem()

    //         substWksp.satisfyGoal(selectedGoal, new substWksp.ShapeFramework(selectedCell.neutralNCell))

    //         // // If the remaining number of goals is zero, delete the workspace
    //         // // and reselect the parent ...

    //         // if (substWksp.isComplete) {
    //         //   println("Substitution finished ... importing results")

    //         //   substWksp.environment.dump

    //         //   substWksp.getImports foreach (expr => {
    //         //     if (! substWksp.parentWorkspace.environment.containsId(expr.value.id)) {
    //         //       println("Importing required cell " ++ expr.value.id)
    //         //       substWksp.parentWorkspace.environment += expr
    //         //     } else {
    //         //       println("Skipping import of " ++ expr.value.id ++ " because of name clash.")
    //         //     }
    //         //   })

    //         //   val parentItem = substWksp.treeItem.parent()
    //         //   parentItem.children -= substWksp.treeItem
    //         //   navigationTreeView.getSelectionModel.select(parentItem)
    //         // }
    //       }
    //     }
    //   }
    // }

  def onUnfold = ???
    // for {
    //   wksp <- activeWorkspace
    // } {
    //   wksp.unfoldSelectedApplication
    // }

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

  def activeGallery : Option[JavaFXWorkspace#WorksheetGallery] = 
    for { wksp <- activeWorkspace ; gallery <- wksp.activeGallery } yield gallery

  def activeSheet : Option[Workspace#Worksheet] = 
    for { 
      wksp <- activeWorkspace 
      sheet <- wksp.activeSheet
    } yield sheet

  def activeDefinition : Option[Definition] = {
    val selectedDefnItem = definitionTreeView.getSelectionModel.selectedItem()

    if (selectedDefnItem != null) {
      Some(findParentDefinition(selectedDefnItem))
    } else None
  }

  def newDefinition(name : String, stabilityLevel : Option[Int], invertibilityLevel : Option[Int], unicityLevel : Option[Int]) = {
    val wksp = new JavaFXDefinitionWorkspace(thisEditor, name, stabilityLevel, invertibilityLevel, unicityLevel)

    navigationTreeRoot.children += wksp.treeItem
    navigationTreeView.getSelectionModel.select(wksp.treeItem)

    wksp.newSheet
  }

  def selectWorkspace(wksp : JavaFXWorkspace) = {
    sheetPane.content = wksp.sheetTabPane
    contextPane.content = wksp.contextView

    if (wksp.isInstanceOf[JavaFXSubstitutionWorkspace]) {
      val substWksp = wksp.asInstanceOf[JavaFXSubstitutionWorkspace]
      substContextPane.content = substWksp.substContextView
    } else
      substContextPane.content = noSubstContextLabel

    activeWorkspace = Some(wksp)
  }

  def closeActiveWorkspace =
    for { wksp <- activeWorkspace } { closeWorkspace(wksp) }

  def closeWorkspace(wksp : JavaFXWorkspace) = {
    val parent = wksp.treeItem.parent()

    if (parent != null) {
      parent.children.remove(wksp.treeItem)
    }

    contextPane.content = noContextLabel
    sheetPane.content.clear
    activeWorkspace = None
  }

  def setPreviewGallery[A](gallery : SpinnerGallery[A]) = {
    previewPane.content += gallery
    gallery.refreshAll
  }

  def clearDefinitions = {
    definitionTreeRoot.children.clear
  }

  def definitions : Seq[Definition] = {
    definitionTreeRoot.children map (child => {
      child.value().asInstanceOf[DefinitionItem].defn
    })
  }

  def findParentDefinition(treeItem : TreeItem[DefinitionTreeItem]) : Definition = {
    treeItem.value() match {
      case DefinitionItem(defn) => defn
      case _ => findParentDefinition(treeItem.parent())
    }
  }

  def addLocalDefinition(defn : Definition) = {
    val defnTreeItem = new TreeItem[DefinitionTreeItem] {
      value = DefinitionItem(defn)
      // children ++= defn.context map (expr => {
      //   new TreeItem[DefinitionTreeItem] {
      //     value = ExpressionItem(expr)
      //   }.delegate
      // })
    }

    definitionTreeRoot.children += defnTreeItem
  }

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
