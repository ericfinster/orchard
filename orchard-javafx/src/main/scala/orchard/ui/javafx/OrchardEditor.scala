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

import orchard.core.cell._
import orchard.core.editor._
import orchard.core.expression._

import orchard.ui.javafx.controls._

trait JavaFXEditor extends Editor {

  implicit def pm : PopupManager

  def newWorkspace(name : String, stabilityLevel : Option[Int], invertibilityLevel : Option[Int], unicityLevel : Option[Int]) : JavaFXWorkspace

  def setPreviewGallery[A](gallery : SpinnerGallery[A]) : Unit
  def setGoalPreviewGallery[A](gallery : SpinnerGallery[A]) : Unit

  def activeWorkspace : Option[JavaFXWorkspace]
  def newModule(name : String)

}

object OrchardEditor extends PopupManager(new VBox) 
    with JavaFXEditor 
    with OrchardMenus 
    with OrchardDialogs { thisEditor =>

  implicit def pm = thisEditor

  val mainVBox = new VBox(root.delegate.asInstanceOf[jfxsl.VBox])

  val fileChooser = new FileChooser

  val workspaceListView = 
    new ListView[JavaFXWorkspace] {
      cellFactory = (_ => new WorkspaceListCell)
    }

  class WorkspaceListCell extends jfxsc.ListCell[JavaFXWorkspace] {

    getStyleClass add "orch-list-cell"

    override def updateItem(wksp : JavaFXWorkspace, empty : Boolean) = {
      super.updateItem(wksp, empty)

      if (! empty) {
        setText(wksp.name)
      }
    }
  }
  
  workspaceListView.selectionModel().selectedItem onChange {
    val item = workspaceListView.selectionModel().getSelectedItem
    
    if (item != null) {
      selectWorkspace(item)
    }
  }

  sealed trait ModuleTreeItem
  case class ModuleItem(val name : String) extends ModuleTreeItem { override def toString = name }
  case class DefinitionItem(val defn : Definition) extends ModuleTreeItem { override def toString = defn.name }
  case class EnvironmentItem(val el : EnvironmentElement) extends ModuleTreeItem

  class ModuleTreeCell extends jfxsc.TreeCell[ModuleTreeItem] {

    getStyleClass add "orch-list-cell"
    val cellStyleIndex = getStyleClass.length
    getStyleClass add "orch-list-null"

    def setCellStyleType(str : String) = {
      getStyleClass(cellStyleIndex) = str
    }

    override def updateItem(item : ModuleTreeItem, empty : Boolean) = {
      super.updateItem(item, empty)

      if (! empty) {
        item match {
          case ModuleItem(name) => { setCellStyleType("orch-list-null") ; setText(name) }
          case DefinitionItem(defn) => { setCellStyleType("orch-list-null") ; setText(defn.name) }
          case EnvironmentItem(GroupElement(name)) => { setCellStyleType("orch-list-null") ; setText(name) }
          case EnvironmentItem(ExpressionElement(expr)) => {
            setCellStyleType("orch-list-cell-" ++ expr.value.styleString)
            setText(expr.value.id)
          }
        }
      } else {
        setCellStyleType("orch-list-null")
        setText("")
      }
    }

  }

  val moduleRoot = new TreeItem[ModuleTreeItem]
  val moduleTreeView = 
    new TreeView[ModuleTreeItem] {
      root = moduleRoot
      showRoot = false
      cellFactory = (_ => new ModuleTreeCell)
    }

  moduleTreeView.selectionModel().selectedItem onChange {
    val item = moduleTreeView.selectionModel().getSelectedItem
    
    def setModuleDefinitionInfo(itm : TreeItem[ModuleTreeItem]) : Unit = {

      def isDefinitionItem(i : TreeItem[ModuleTreeItem]) = 
        i.value().isInstanceOf[DefinitionItem]

      var curItem = itm

      while (! curItem.value().isInstanceOf[DefinitionItem]) { 
        curItem = curItem.parent() 
      }

      activeDefinition = Some(curItem.value().asInstanceOf[DefinitionItem].defn)
      activeModuleItem = Some(curItem.parent())
    }

    if (item != null) {
      item.value() match {
        case m @ ModuleItem(name) => activeModuleItem = Some(item)
        case t @ DefinitionItem(defn) => {
          activeModuleItem = Some(item.parent())
          activeDefinition = Some(defn)
        }
        case e @ EnvironmentItem(ExpressionElement(expr)) => { setPreview(expr) ; setModuleDefinitionInfo(item) }
        case e @ EnvironmentItem(_) => setModuleDefinitionInfo(item)
      }
    }
  }

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

  val modulePane = new TitledPane {
    text = "Modules"
    content = moduleTreeView
    collapsible = false
  }

  val moduleAnchor = new AnchorPane {
    content = modulePane
    styleClass += "orch-pane"
  }

  AnchorPane.setTopAnchor(modulePane, 10)
  AnchorPane.setRightAnchor(modulePane, 10)
  AnchorPane.setBottomAnchor(modulePane, 10)
  AnchorPane.setLeftAnchor(modulePane, 10)

  val noEnvLabel = new Label("Empty Environment")

  val environmentPane = new TitledPane {
    text = "Environment"
    content = noEnvLabel
    collapsible = false
  }

  val environmentAnchor = new AnchorPane {
    content = environmentPane
    styleClass += "orch-pane"
  }

  AnchorPane.setTopAnchor(environmentPane, 10)
  AnchorPane.setRightAnchor(environmentPane, 10)
  AnchorPane.setBottomAnchor(environmentPane, 10)
  AnchorPane.setLeftAnchor(environmentPane, 10)

  val substitutionPane = new StackPane {
    styleClass += "orch-pane"
  }

  val substitutionAnchor = new AnchorPane {
    content = substitutionPane
    styleClass += "orch-pane"
  }

  AnchorPane.setTopAnchor(substitutionPane, 10)
  AnchorPane.setRightAnchor(substitutionPane, 10)
  AnchorPane.setBottomAnchor(substitutionPane, 10)
  AnchorPane.setLeftAnchor(substitutionPane, 10)

  val goalPane = new StackPane {
    padding = Insets(10,10,10,10)
    styleClass += "orch-pane"
  }

  val sheetPane = new StackPane {
    padding = Insets(10,10,10,10)
    styleClass += "orch-pane"
  }

  val previewPane = new StackPane {
    padding = Insets(0,10,0,10)
    styleClass += "orch-pane"
  }

  val goalPreviewPane = new StackPane {
    padding = Insets(0,10,0,10)
    styleClass += "orch-pane"
  }

  val previewRow = new RowConstraints { percentHeight = 50 }
  val goalRow = new RowConstraints { percentHeight = 50 }

  val previewGridPane = new GridPane {
    rowConstraints = List(previewRow, goalRow)
    styleClass += "orch-pane"
  }

  GridPane.setHgrow(previewPane, Priority.ALWAYS)
  GridPane.setHgrow(goalPreviewPane, Priority.ALWAYS)

  previewGridPane.add(previewPane, 0, 0)
  previewGridPane.add(goalPreviewPane, 0, 1)

  val leftVerticalSplit = new SplitPane {
    orientation = Orientation.VERTICAL
    items.addAll(workspaceAnchor, moduleAnchor)
  }

  val middleVerticalSplit = new SplitPane {
    orientation = Orientation.VERTICAL
    items.addAll(sheetPane, previewGridPane)
  }

  middleVerticalSplit.setDividerPositions(0.6f)

  val rightVerticalSplit = new SplitPane {
    orientation = Orientation.VERTICAL
    items.addAll(environmentAnchor, substitutionAnchor)
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

            ev.consume
          }
          case KeyCode.RIGHT => {
            if (ev.isControlDown) {
              val previewGallery = previewPane.content.head.asInstanceOf[SpinnerGallery[Any]]
              if (previewGallery != null)
                previewGallery.next
            } else 
              for { gallery <- activeGallery } gallery.next

            ev.consume
          }
          case KeyCode.E => if (ev.isControlDown) onExtrude
          case KeyCode.D => if (ev.isControlDown) onDrop
          case KeyCode.A => if (ev.isControlDown) onAssume(ev.isShiftDown)
          case KeyCode.F => if (ev.isControlDown) onFill  
          case KeyCode.P => if (ev.isControlDown) onPaste
          case KeyCode.T => if (ev.isControlDown) onNewSheet
          case KeyCode.O => if (ev.isControlDown) onOpenModule
          case KeyCode.S => if (ev.isControlDown) onSaveModule
          case KeyCode.B => if (ev.isControlDown) onBind
          case KeyCode.N => if (ev.isControlDown) onNewWorkspace
          case KeyCode.M => if (ev.isControlDown) onNewModule
          case KeyCode.I => if (ev.isControlDown) onImportSubstitution
          case KeyCode.V => if (ev.isControlDown) { if (ev.isShiftDown) onNewSubstInShell else onNewSubstitution }
          case KeyCode.X => if (ev.isControlDown) onCloseWorkspace 
          case KeyCode.L => if (ev.isControlDown) onAbstract
          case KeyCode.W => if (ev.isControlDown) onCancelSubstitution
          case KeyCode.R => if (ev.isControlDown) onRename
          case KeyCode.U => if (ev.isControlDown) onUnify(ev.isShiftDown)
          case KeyCode.G => if (ev.isControlDown) onGetEnvironmentCell
  //         // case KeyCode.V => if (ev.isControlDown) onView
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

  def onExit : Unit = javafx.application.Platform.exit

  def onNewModule = {
    val moduleDialog = new NewModuleDialog
    moduleDialog.run
  }

  def onOpenModule = {
    fileChooser.setTitle("Open")

    val file = fileChooser.showOpenDialog(getScene.getWindow)

    if (file != null) {
      loadModule(file)
    }
  }

  def onSaveModule = {
    fileChooser.setTitle("Save")

    val file = fileChooser.showSaveDialog(getScene.getWindow)

    if (file != null) {
      saveActiveModule(file)
    }
  }

  def onCreateDefinition =
    for {
      wksp <- activeWorkspace
      moduleItem <- activeModuleItem
    } {
      println("Creating definition from workspace ...")
      addDefinitionToModule(wksp.definitionSnapshot, moduleItem)
    }

  def onDeleteDefinition =
    for {
      moduleItem <- activeModuleItem
      definition <- activeDefinition
    } {
      val definitionItem = 
        (moduleItem.children find (child => child.value().asInstanceOf[DefinitionItem].defn == definition)).get

      moduleItem.children -= definitionItem
    }

  def onNewWorkspace = NewWorkspaceDialog.run
  def onOpenDefinition = 
    for {
      defn <- activeDefinition
    } {
      val wksp = newWorkspace(defn.name, defn.stabilityLevel, defn.invertibilityLevel, defn.unicityLevel)
      val env = wksp.envOps.cloneFrom(defn.envRoot, defn.envOps)
      wksp.envOps.children(wksp.envRoot) ++= wksp.envOps.children(env)
    }

  def onCloseWorkspace = for { wksp <- activeWorkspace } { closeWorkspace(wksp) }
  def onNewSheet = for { wksp <- activeWorkspace } { wksp.newSheet }

  def onNewSubstitution = 
    for {
      wksp <- activeWorkspace
      defn <- activeDefinition
    } {
      wksp.newSubstitution(defn)
    }

  def onNewSubstInShell =
    for {
      wksp <- activeWorkspace
      defn <- activeDefinition
    } {
      wksp.newSubstitutionInSelectedShell(defn)
    }

  def onImportSubstitution =
    for {
      wksp <- activeWorkspace
    } {
      wksp.importActiveSubstitution
    }

  def onCancelSubstitution = 
    for { 
      wksp <- activeWorkspace 
    } {
      wksp.cancelActiveSubstitution
    }

  def onGetEnvironmentCell = 
    for {
      wksp <- activeWorkspace
    } {
      val idDialog = 
        new SimpleIdentifierDialog(wksp.selectEnvironmentCell) { 
          heading.text = "Find Cell by Identifier"
        }

      idDialog.run
    }

  def onBind =
    for {
      wksp <- activeWorkspace
      expr <- wksp.activeExpression
      subst <- wksp.activeSubstitution
      varExpr <- subst.activeExpression
    } {
      if (varExpr.value.isInstanceOf[Variable]) {
        subst.bindVariable(varExpr, expr) 
      } else {
        println("Selected expression is not a variable.")
      }
    }

  def onAbstract =
    for {
      wksp <- activeWorkspace
      subst <- wksp.activeSubstitution
      expr <- subst.activeExpression
    } {
      expr.value match {
        case Variable(_, _) => println("Cannot abstract a variable.")
        case f @ Filler(_, _, _) => subst.abstractExpression(f)
        case bdry : Filler#Boundary => subst.abstractExpression(bdry.interior)
      }
    }

  def onUnify(unifyVariables : Boolean) =
    for {
      wksp <- activeWorkspace
      subst <- wksp.activeSubstitution
    } {
      println("Unifying substitution with environment expressions ...")
      subst.unifyFillers(unifyVariables)
    }

  def onAssume(thinHint : Boolean) : Unit = for { wksp <- activeWorkspace } { wksp.assumeAtSelection(thinHint) }
  def onFill : Unit = for { wksp <- activeWorkspace } { wksp.fillAtSelection }
  def onPaste : Unit = for { wksp <- activeWorkspace } { wksp.pasteToSelection }
  def onRename : Unit = for { wksp <- activeWorkspace } { wksp.renameActiveExpression }

  def onExtrude : Unit = for { wksht <- activeSheet } { wksht.extrude }
  def onDrop : Unit = for { wksht <- activeSheet } { wksht.drop }

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

  def withRenameIdentifier(expr : Expression, handler : String => Unit) : Unit = {
    val renameDialog = new RenameDialog(expr, handler)
    renameDialog.run
  }

  var activeWorkspace : Option[JavaFXWorkspace] = None
  var activeModuleItem : Option[TreeItem[ModuleTreeItem]] = None
  var activeDefinition : Option[Definition] = None

  def activeGallery : Option[JavaFXWorkspace#WorksheetGallery] =
    for { wksp <- activeWorkspace ; gallery <- wksp.activeGallery } yield gallery

  def activeSheet : Option[JavaFXWorkspace#Worksheet] = 
    for { 
      wksp <- activeWorkspace 
      sheet <- wksp.activeSheet
    } yield sheet

  def newWorkspace(name : String, stabilityLevel : Option[Int], invertibilityLevel : Option[Int], unicityLevel : Option[Int]) : JavaFXWorkspace = {
    val wksp = new JavaFXWorkspace(thisEditor, name, stabilityLevel, invertibilityLevel, unicityLevel)

    workspaceListView.items() += wksp
    workspaceListView.getSelectionModel.select(wksp)

    wksp.newSheet
    wksp
  }

  def selectWorkspace(wksp : JavaFXWorkspace) = {
    sheetPane.content = wksp.sheetTabPane
    environmentPane.content = wksp.environmentView
    substitutionPane.content = wksp.substitutionAccordion
    activeWorkspace = Some(wksp)
  }

  def closeWorkspace(wksp : JavaFXWorkspace) = {
    workspaceListView.items() -= wksp
    environmentPane.content = noEnvLabel
    sheetPane.content.clear
    activeWorkspace = None
  }

  def setPreview(expr : NCell[Expression]) = {
    setPreviewGallery(new FrameworkGallery(expr map (Some(_))))
  }

  def setPreviewGallery[A](gallery : SpinnerGallery[A]) = {
    previewPane.content.clear
    previewPane.content += gallery
    gallery.refreshAll
  }

  def setGoalPreviewGallery[A](gallery : SpinnerGallery[A]) = {
    goalPreviewPane.content.clear
    goalPreviewPane.content += gallery
    gallery.refreshAll
  }

  def modules : Seq[ModuleItem] = 
    moduleRoot.children map (child => child.asInstanceOf[ModuleItem])

  def newModule(name : String) = {
    val moduleItem = new TreeItem[ModuleTreeItem] { value = ModuleItem(name) }
    moduleRoot.children += moduleItem
  }

  def buildDefinitionTreeItems(node : SimpleEnvironmentNode) : TreeItem[ModuleTreeItem] = {
    val item = new TreeItem[ModuleTreeItem](EnvironmentItem(node.element))
    item.children ++= node.children map (buildDefinitionTreeItems(_).delegate)
    item
  }
  
  def addDefinitionToModule(defn : Definition, moduleItem : TreeItem[ModuleTreeItem]) = {
    val treeItem = buildDefinitionTreeItems(defn.envRoot)
    treeItem.value = DefinitionItem(defn)
    moduleItem.children += treeItem.delegate
  }

  def saveActiveModule(file : java.io.File) = 
    for {
      moduleItem <- activeModuleItem
    } {
      val name = moduleItem.value().asInstanceOf[ModuleItem].name
      val defns = moduleItem.children map (child => child.value().asInstanceOf[DefinitionItem].defn)
      val moduleXML = <module name={name}>{defns map (defn => Definition.definitionToXML(defn))}</module>
      xml.XML.save(file.getAbsolutePath, moduleXML)
    }

  def loadModule(file : java.io.File) = {
    val elem = xml.XML.loadFile(file.getAbsolutePath)

    elem match {
      case m @ <module>{defns @ _*}</module> => {
        val name = (m \ "@name").text
        val moduleItem = 
          new TreeItem[ModuleTreeItem] {
            value = ModuleItem(name)
          }

        defns foreach (defn => {
          addDefinitionToModule(Definition.fromXML(defn), moduleItem)
        })    

        moduleRoot.children += moduleItem
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
