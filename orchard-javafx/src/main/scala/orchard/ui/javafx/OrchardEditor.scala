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

trait JavaFXEditor extends Editor {

  implicit def pm : PopupManager

  def newWorkspace(name : String, stabilityLevel : Option[Int], invertibilityLevel : Option[Int], unicityLevel : Option[Int]) : Unit

}

object OrchardEditor extends PopupManager(new VBox) 
    with JavaFXEditor 
    with OrchardMenus 
    with OrchardDialogs { thisEditor =>

  implicit def pm = thisEditor

  val mainVBox = new VBox(root.delegate.asInstanceOf[jfxsl.VBox])

  val fileChooser = new FileChooser

  val sheetPane = new StackPane {
    padding = Insets(10,10,10,10)
    styleClass += "orch-pane"
  }

  val environmentListView = 
    new ListView[NCell[Expression]] {
      items = ObservableBuffer.empty[NCell[Expression]]
      cellFactory = (_ => new EnvironmentCell { envCell =>
        setOnMouseClicked(new EventHandler[MouseEvent] {
          def handle(ev : MouseEvent) {
            val item = envCell.item()

            if (item != null) {
              if (ev.getClickCount > 1) {
                for { wksp <- activeWorkspace } { wksp.newSheet(item map (Some(_))) }
              }
            }
          }
        })
      })
    }

  environmentListView.getSelectionModel.selectedItem onChange {
    val item = environmentListView.getSelectionModel.selectedItem()

    for { wksp <- activeWorkspace } {
      if (item != null) {
        wksp.activeExpression = Some(item)
        setPreview(item)
      } else {
        wksp.activeExpression = None
      }
    }
  }

  val environmentPane = new TitledPane {
    text = "Environment"
    content = environmentListView
  }

  val goalsListView = 
    new ListView[GoalComplex] {
      items = ObservableBuffer.empty[GoalComplex]
      cellFactory = (_ => new GoalListCell)
    }

  goalsListView.getSelectionModel.selectedItem onChange {
    val item = goalsListView.getSelectionModel.selectedItem()

    if (item != null) {
      // setPreview(item)
    }
  }

  val goalsPane = new TitledPane {
    text = "Goals"
    content = goalsListView
  }

  val localAccordion = new Accordion {
    panes = List(environmentPane, goalsPane)
    expandedPane = environmentPane
  }

  sealed trait NavigationTreeItem
  case class DefnWorkspaceItem(val wksp : JavaFXDefinitionWorkspace) extends NavigationTreeItem
  case class SubstWorkspaceItem(val wksp : JavaFXSubstitutionWorkspace) extends NavigationTreeItem

  val navigationTreeRoot = new TreeItem[NavigationTreeItem]
  val navigationTreeView = 
    new TreeView[NavigationTreeItem] {
      root = navigationTreeRoot
      showRoot = false
      cellFactory = (_ => new NavigationTreeCell)
    }

  navigationTreeView.selectionModel().selectedItem onChange {
    val item = navigationTreeView.selectionModel().getSelectedItem
    
    if (item != null) {
      item.value() match {
        case DefnWorkspaceItem(wksp) => selectWorkspace(wksp)
        case SubstWorkspaceItem(wksp) => selectWorkspace(wksp)
        case _ => println("Don't know what to do with this.")
      }
    }
  }

  val navigationPane = new TitledPane {
    text = "Navigation"
    content = navigationTreeView
  }

  class NavigationTreeCell extends jfxsc.TreeCell[NavigationTreeItem] {

    getStyleClass add "orch-list-cell"

    var lastStyle : Option[String] = None

    def setStyleType(styleType : String) = {
      lastStyle foreach (st => getStyleClass remove st)
      getStyleClass add styleType
      lastStyle = Some(styleType)
    }

    def clearStyleType = {
      lastStyle foreach (st => getStyleClass remove st)
      lastStyle = None
    }

    override def updateItem(navTreeItem : NavigationTreeItem, empty : Boolean) = {
      super.updateItem(navTreeItem, empty)

      if (! empty) {
        navTreeItem match {
          case DefnWorkspaceItem(wksp) => { clearStyleType ; setText(wksp.name) }
          case SubstWorkspaceItem(wksp) => { clearStyleType ; setText(wksp.name) }
          case item @ _ => { clearStyleType ; setText(item.toString) }
        }
      }
    }
  }

  sealed trait DefinitionTreeItem
  case class DefinitionItem(val defn : Definition) extends DefinitionTreeItem { override def toString = defn.toString }
  case class ExpressionItem(val expr : NCell[Expression]) extends DefinitionTreeItem { override def toString = expr.value.id }

  class DefinitionTreeCell extends jfxsc.TreeCell[DefinitionTreeItem] {

    getStyleClass add "orch-list-cell"

    var lastStyle : Option[String] = None

    def setStyleType(styleType : String) = {
      lastStyle foreach (st => getStyleClass remove st)
      getStyleClass add styleType
      lastStyle = Some(styleType)
    }

    def clearStyleType = {
      lastStyle foreach (st => getStyleClass remove st)
      lastStyle = None
    }

    override def updateItem(defnTreeItem : DefinitionTreeItem, empty : Boolean) = {
      super.updateItem(defnTreeItem, empty)

      if (! empty) {
        defnTreeItem match {
          case DefinitionItem(defn) => { clearStyleType ; setText(defn.toString) }
          case ExpressionItem(expr) => {
            expr.value match {
              case Variable(_, isThin) => {
                if (isThin) {
                  setStyleType("orch-list-cell-var-thin")
                } else {
                  setStyleType("orch-list-cell-var")
                }
              }
              case Filler(_) => setStyleType("orch-list-cell-filler")
              case FillerFace(_, _, isThin) => {
                if (isThin) {
                  setStyleType("orch-list-cell-filler-face-thin")
                } else {
                  setStyleType("orch-list-cell-filler-face")
                }
              }
              case UnicityFiller(_) => setStyleType("orch-list-cell-ufiller")
            }

            setText(expr.toString)
          }
          case item @ _ => { clearStyleType ; setText(item.toString) }
        }
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

  definitionTreeView.selectionModel().selectedItem onChange {
    val item = definitionTreeView.selectionModel().getSelectedItem
    
    if (item != null) {
      item.value() match {
        case ExpressionItem(expr) => setPreview(expr)
        case _ => ()
      }
    }
  }

  val definitionsPane = new TitledPane {
    text = "Local Definitions"
    content = definitionTreeView
  }

  val accordion = new Accordion {
    panes = List(navigationPane, definitionsPane)
    expandedPane = navigationPane
  }

  val accordionPane = new AnchorPane {
    content = accordion
    styleClass += "orch-pane"
  }

  AnchorPane.setTopAnchor(accordion, 10)
  AnchorPane.setRightAnchor(accordion, 10)
  AnchorPane.setBottomAnchor(accordion, 10)
  AnchorPane.setLeftAnchor(accordion, 10)

  val localAnchorPane = new AnchorPane {
    content = localAccordion
    styleClass += "orch-pane"
  }

  AnchorPane.setTopAnchor(localAccordion, 10)
  AnchorPane.setRightAnchor(localAccordion, 10)
  AnchorPane.setBottomAnchor(localAccordion, 10)
  AnchorPane.setLeftAnchor(localAccordion, 10)

  val navigationSplit = new SplitPane {
    orientation = Orientation.VERTICAL
    items.addAll(accordionPane, localAnchorPane)
  }

  val previewPane = new StackPane {
    styleClass += "orch-pane"
    padding = Insets(0,10,0,10)
  }

  val verticalSplit = new SplitPane {
    orientation = Orientation.VERTICAL
    items.addAll(sheetPane, previewPane)
    dividerPositions = 0.7f
  }

  val horizontalSplit = new SplitPane {
    orientation = Orientation.HORIZONTAL
    items.addAll(navigationSplit, verticalSplit)
    dividerPositions = 0.1f
  }

  VBox.setVgrow(horizontalSplit, Priority.ALWAYS)
  mainVBox.content.addAll(menuBar, horizontalSplit)

  //============================================================================================
  // WORKSPACE DEFINITIONS
  //

  trait JavaFXWorkspace extends Workspace {

    type EnvironmentSeqType = ObservableBuffer[NCell[Expression]]

    def editor = thisEditor

    def treeItem : TreeItem[NavigationTreeItem]

    var sheetCount : Int = 1

    val sheetTabPane = new TabPane {
      side = Side.TOP
    }

    val environment = ObservableBuffer.empty[NCell[Expression]]

    var activeGallery : Option[JavaFXWorksheetGallery] = None
    var activeExpression : Option[NCell[Expression]] = None

    def activeSheet : Option[ExpressionWorksheet] =
      for {
        gallery <- activeGallery
      } yield gallery.complex

    def newSheet = newSheet(Object(None))

    def newSheet(seed : NCell[Option[Expression]]) : Unit = {
      val gallery = new JavaFXWorksheetGallery(CardinalComplex(seed))

      val tab = new Tab {
        text = "Sheet " ++ sheetCount.toString
        content = gallery

        onClosed = () => { 
          sheets -= gallery.complex
        }

        onSelectionChanged = () => { 
          if (selected())
            activeGallery = Some(gallery)
        }
      }

      sheetTabPane += tab
      sheetTabPane.getSelectionModel.select(tab)
      sheets += gallery.complex
      sheetCount += 1
      gallery.refreshAll
    }
  }

  class JavaFXDefinitionWorkspace(
    val name : String,
    val stabilityLevel : Option[Int],
    val invertibilityLevel : Option[Int],
    val unicityLevel : Option[Int]
  ) extends DefinitionWorkspace with JavaFXWorkspace { thisWksp =>

    val treeItem = new TreeItem[NavigationTreeItem] {
      value = DefnWorkspaceItem(thisWksp)
    }

  }

  class JavaFXSubstitutionWorkspace(
    val name : String,
    val parentWorkspace : Workspace,
    val defn : Definition,
    val shell : SimpleFramework
  ) extends SubstitutionWorkspace with JavaFXWorkspace { thisWksp =>

    val treeItem = new TreeItem[NavigationTreeItem] {
      value = SubstWorkspaceItem(thisWksp)
    }

    def stabilityLevel : Option[Int] = parentWorkspace.stabilityLevel
    def invertibilityLevel : Option[Int] = parentWorkspace.invertibilityLevel
    def unicityLevel : Option[Int] = parentWorkspace.unicityLevel

  }

  //============================================================================================
  // EVENTS
  //

  addEventFilter(KeyEvent.KEY_PRESSED,
    new EventHandler[KeyEvent] {
      def handle(ev : KeyEvent) {
        ev.getCode match {
          case KeyCode.LEFT => {
            if (ev.isControlDown) {
              val previewGallery = previewPane.content.head.asInstanceOf[FrameworkGallery]
              if (previewGallery != null)
                previewGallery.prev
            } else
              for { gallery <- activeGallery } gallery.prev
          }
          case KeyCode.RIGHT => {
            if (ev.isControlDown) {
              val previewGallery = previewPane.content.head.asInstanceOf[FrameworkGallery]
              if (previewGallery != null)
                previewGallery.next
            } else 
              for { gallery <- activeGallery } gallery.next
          }
          case KeyCode.E => if (ev.isControlDown) onExtrude
          case KeyCode.D => if (ev.isControlDown) onDrop
          case KeyCode.A => if (ev.isControlDown) onAssume(ev.isShiftDown)
          case KeyCode.C => if (ev.isControlDown) onCompose
          case KeyCode.I => if (ev.isControlDown) onInsertIdentity
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
  def onCompose : Unit = for { wksp <- activeWorkspace } { wksp.composeAtSelection }
  def onInsertIdentity : Unit = for { wksp <- activeWorkspace } { wksp.identityAtSelection }
  def onFill : Unit = for { wksp <- activeWorkspace } { wksp.fillAtSelection }
  def onUseEnvironment : Unit = for { wksp <- activeWorkspace } { wksp.expressionToSelection }

  def onNewDefinition = NewDefinitionDialog.run
  def onNewSheet = for { wksp <- activeWorkspace } { wksp.newSheet }

  def onCompleteDefinition = {
    for {
      wksp <- activeWorkspace
      expr <- wksp.activeExpression
    } {
      if (wksp.isInstanceOf[DefinitionWorkspace]) {
        val defnWksp = wksp.asInstanceOf[DefinitionWorkspace]

        val defn =
          new Definition(defnWksp.name,
            defnWksp.stabilityLevel,
            defnWksp.invertibilityLevel,
            defnWksp.unicityLevel,
            wksp.environment)

        addLocalDefinition(defn)
        closeActiveWorkspace
        accordion.expandedPane = definitionsPane
      }
    }
  }

  def onSpawnInShell = 
    for { 
      wksp <- activeWorkspace
      gallery <- wksp.activeGallery
      cell <- gallery.complex.selectionBase
    } {
      if (cell.hasCompleteShell) {
        // Now we need to find the definition we are spawning.

        val selectedWkspItem = navigationTreeView.getSelectionModel.selectedItem()
        val selectedDefnItem = definitionTreeView.getSelectionModel.selectedItem()

        if (selectedDefnItem != null && selectedWkspItem != null) {
          val selectedDefn = findParentDefinition(selectedDefnItem)
          val selectedShell = cell.getSimpleFramework

          val substWksp = 
            new JavaFXSubstitutionWorkspace(selectedDefn.name, 
                                            wksp,
                                            selectedDefn,
                                            selectedShell)

          // Import the current environment and create a blank sheet
          substWksp.environment ++= wksp.environment

          // We should generate more tree items for the goals now

          selectedWkspItem.children += substWksp.treeItem
          navigationTreeView.getSelectionModel.select(substWksp.treeItem)

          substWksp.newSheet
        } else {
          println("No definition selected.")
        }
      } else {
        println("Cannot spawn substitution in incomplete shell.")
      }
    }

  def onSatisfyGoal = {
  }

  def onOpen = {
    fileChooser.setTitle("Open")

    val file = fileChooser.showOpenDialog(getScene.getWindow)

    if (file != null) {
      loadDefinitions(file)
    }
  }

  def onSave = {
    fileChooser.setTitle("Save")

    val file = fileChooser.showSaveDialog(getScene.getWindow)

    if (file != null) {
      saveDefinitions(file)
    }
  }

  def onExit : Unit = javafx.application.Platform.exit

  //============================================================================================
  // EDITOR HELPER ROUTINES
  //

  def withAssumptionInfo(deps : Seq[NCell[Expression]],
                         thinHint : Boolean,
                         forceThin : Boolean,
                         handler : (String, Boolean) => Unit) : Unit = {
    val varDialog = new VariableDialog(handler)

    if (thinHint || forceThin) varDialog.thinCheckBox.selected = true
    if (forceThin) varDialog.thinCheckBox.disable = true

    varDialog.dependenciesList.items = ObservableBuffer(deps)

    varDialog.run
  }

  def withFillerIdentifiers(deps : Seq[NCell[Expression]],
    handler : (String, String) => Unit) : Unit = {
    val fillingDialog = new FillingDialog(handler)
    fillingDialog.dependenciesList.items = ObservableBuffer(deps)
    fillingDialog.run
  }


  def withFillerIdentifier(deps : Seq[NCell[Expression]], handler : String => Unit) : Unit = {
    val uniqueFillingDialog = new UniqueFillingDialog(handler)
    uniqueFillingDialog.dependenciesList.items = ObservableBuffer(deps)
    uniqueFillingDialog.run
  }

  var activeWorkspace : Option[JavaFXWorkspace] = None

  def activeGallery : Option[JavaFXWorksheetGallery] = 
    for { wksp <- activeWorkspace ; gallery <- wksp.activeGallery } yield gallery

  def activeSheet : Option[ExpressionWorksheet] = 
    for { 
      wksp <- activeWorkspace 
      sheet <- wksp.activeSheet
    } yield sheet

  def newWorkspace(name : String, stabilityLevel : Option[Int], invertibilityLevel : Option[Int], unicityLevel : Option[Int]) = {
    val wksp = new JavaFXDefinitionWorkspace(name, stabilityLevel, invertibilityLevel, unicityLevel)

    navigationTreeRoot.children += wksp.treeItem
    navigationTreeView.getSelectionModel.select(wksp.treeItem)

    wksp.newSheet
  }

  def selectWorkspace(wksp : JavaFXWorkspace) = {
    sheetPane.content = wksp.sheetTabPane
    environmentListView.items = wksp.environment

    goalsListView.items = 
      if (wksp.isInstanceOf[SubstitutionWorkspace]) {
        ObservableBuffer(wksp.asInstanceOf[SubstitutionWorkspace].goals)
      } else
        ObservableBuffer.empty[GoalComplex]

    activeWorkspace = Some(wksp)
  }

  def closeActiveWorkspace =
    for { wksp <- activeWorkspace } { closeWorkspace(wksp) }

  def closeWorkspace(wksp : JavaFXWorkspace) = {
    val parent = wksp.treeItem.parent()

    if (parent != null) {
      parent.children.remove(wksp.treeItem)
    }

    environmentListView.items().clear
    sheetPane.content.clear
    activeWorkspace = None
  }

  def setPreview(expr : NCell[Expression]) = {
    val gallery = new FrameworkGallery(expr map (e => Some(e)))
    gallery.renderAll
    previewPane.content = gallery
    previewPane.requestLayout
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
      children ++= defn.environment map (expr => {
        new TreeItem[DefinitionTreeItem] {
          value = ExpressionItem(expr)
        }.delegate
      })
    }

    definitionTreeRoot.children += defnTreeItem
  }

  def saveDefinitions(file : java.io.File) = {
    import XmlSerializable._

    val moduleXML = <module>{definitions map (defn => definitionSerializable.toXML(defn))}</module>
    xml.XML.save(file.getAbsolutePath, moduleXML)
  }

  def loadDefinitions(file : java.io.File) = {
    import XmlSerializable._

    clearDefinitions

    val elem = xml.XML.loadFile(file.getAbsolutePath)

    elem match {
      case <module>{defns @ _*}</module> => {
        trimText(defns) foreach (defXml => {
          val defn = definitionSerializable.fromXML(defXml)
          addLocalDefinition(defn)
        })        
      }
    }
  }
}

object Editor extends JFXApp {

  val orchardScene = new Scene(OrchardEditor, 1600, 900)

  orchardScene.getStylesheets.add("orchard/ui/javafx/OrchardUI.css")

  stage = new JFXApp.PrimaryStage {
      title = "Orchard"
      scene = orchardScene
    }

}
