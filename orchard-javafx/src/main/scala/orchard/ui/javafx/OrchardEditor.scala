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

  def newWorkspace(name : String, stabilityLevel : Option[Int], invertibilityLevel : Option[Int], unicityLevel : Option[Int]) : Unit
  def setPreviewGallery[A](gallery : SpinnerGallery[A]) : Unit
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
  case class ModuleItem(name : String) extends ModuleTreeItem { override def toString = name }
  case class TemplateItem(template : Template) extends ModuleTreeItem { override def toString = template.name }
  case class EnvironmentItem(node : EnvironmentNode) extends ModuleTreeItem

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
          case TemplateItem(template) => { setCellStyleType("orch-list-null") ; setText(template.name) }
          case EnvironmentItem(GroupNode(name)) => { setCellStyleType("orch-list-null") ; setText(name) }
          case EnvironmentItem(ExpressionNode(expr)) => {
            setCellStyleType("orch-list-cell-" ++ expr.value.styleString)
            setText(expr.value.toString)
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
    
    def setModuleTemplateInfo(itm : TreeItem[ModuleTreeItem]) : Unit = {

      def isTemplateItem(i : TreeItem[ModuleTreeItem]) = 
        i.value().isInstanceOf[TemplateItem]

      var curItem = itm

      while (! curItem.value().isInstanceOf[TemplateItem]) { 
        curItem = curItem.parent() 
      }

      activeTemplate = Some(curItem.value().asInstanceOf[TemplateItem].template)
      activeModuleItem = Some(curItem.parent())
    }

    if (item != null) {
      item.value() match {
        case m @ ModuleItem(name) => activeModuleItem = Some(item)
        case t @ TemplateItem(template) => {
          activeModuleItem = Some(item.parent())
          activeTemplate = Some(template)
        }
        case e @ EnvironmentItem(ExpressionNode(expr)) => { setPreview(expr) ; setModuleTemplateInfo(item) }
        case e @ EnvironmentItem(_) => setModuleTemplateInfo(item)
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
    items.addAll(workspaceAnchor, moduleAnchor)
  }

  val middleVerticalSplit = new SplitPane {
    orientation = Orientation.VERTICAL
    items.addAll(sheetPane, previewPane)
    dividerPositions = 0.7f
  }

  val horizontalSplit = new SplitPane {
    orientation = Orientation.HORIZONTAL
    items.addAll(leftVerticalSplit, middleVerticalSplit, environmentAnchor)
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
          case KeyCode.O => if (ev.isControlDown) onOpenModule
          case KeyCode.S => if (ev.isControlDown) onSaveModule
          case KeyCode.B => if (ev.isControlDown) onSubstitute
          case KeyCode.N => if (ev.isControlDown) onNewWorkspace
          case KeyCode.M => if (ev.isControlDown) onNewModule
          case KeyCode.I => if (ev.isControlDown) { if (ev.isShiftDown) onImportTemplateInShell else onImportTemplate }
          case KeyCode.X => if (ev.isControlDown) { onCloseWorkspace }
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

  def onExit : Unit = javafx.application.Platform.exit

  def onNewWorkspace = NewWorkspaceDialog.run
  def onCloseWorkspace = for { wksp <- activeWorkspace } { closeWorkspace(wksp) }
  def onNewSheet = for { wksp <- activeWorkspace } { wksp.newSheet }

  def onExportTemplate = 
    for {
      wksp <- activeWorkspace
      moduleItem <- activeModuleItem
    } {
      println("Exporting workspace as template ...")
      addTemplateToModule(wksp.templateSnapshot, moduleItem)
    }

  def onImportTemplate = 
    for {
      wksp <- activeWorkspace
      template <- activeTemplate
    } {
      println("Applying template")
      wksp.importTemplate(template)
    }

  def onImportTemplateInShell = 
    for {
      wksp <- activeWorkspace
      template <- activeTemplate
    } {
      wksp.importTemplateAtSelection(template)
    }

  def onAssume(thinHint : Boolean) : Unit = for { wksp <- activeWorkspace } { wksp.assumeAtSelection(thinHint) }
  def onFill : Unit = for { wksp <- activeWorkspace } { wksp.fillAtSelection }
  def onUseEnvironment : Unit = for { wksp <- activeWorkspace } { wksp.expressionToSelection }

  def onSubstitute =
    for {
      wksp <- activeWorkspace
      varExpr <- wksp.activeExpression
    } {
      if (varExpr.value.isInstanceOf[Variable]) {
        val substDialog = new SubstitutionDialog(varExpr, wksp.buildEnvironmentView)
        substDialog.run
      } else {
        println("Selected expression is not a variable.")
      }
    }

  def onAbstract = 
    for {
      wksp <- activeWorkspace
      expr <- wksp.activeExpression
    } {
      expr.value match {
        case Variable(_, _) => println("Cannot abstract a variable.")
        case f @ Filler(_, _, _) => wksp.abstractExpression(f)
        case bdry : Filler#Boundary => wksp.abstractExpression(bdry.interior)
      }
    }

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

  var activeWorkspace : Option[JavaFXWorkspace] = None
  var activeModuleItem : Option[TreeItem[ModuleTreeItem]] = None
  var activeTemplate : Option[Template] = None

  def activeGallery : Option[JavaFXWorkspace#WorksheetGallery] =
    for { wksp <- activeWorkspace ; gallery <- wksp.activeGallery } yield gallery

  def activeSheet : Option[JavaFXWorkspace#Worksheet] = 
    for { 
      wksp <- activeWorkspace 
      sheet <- wksp.activeSheet
    } yield sheet

  def newWorkspace(name : String, stabilityLevel : Option[Int], invertibilityLevel : Option[Int], unicityLevel : Option[Int]) = {
    val wksp = new JavaFXWorkspace(thisEditor, name, stabilityLevel, invertibilityLevel, unicityLevel)

    workspaceListView.items() += wksp
    workspaceListView.getSelectionModel.select(wksp)

    wksp.newSheet
  }

  def selectWorkspace(wksp : JavaFXWorkspace) = {
    sheetPane.content = wksp.sheetTabPane
    environmentPane.content = wksp.environmentView
    activeWorkspace = Some(wksp)
  }

  def closeWorkspace(wksp : JavaFXWorkspace) = {
    workspaceListView.items() -= wksp
    environmentPane.content = noEnvLabel
    sheetPane.content.clear
    activeWorkspace = None
  }

  def setPreview(expr : NCell[Expression]) = {
    val gallery = new FrameworkGallery(expr map (Some(_)))
    previewPane.content.clear
    previewPane.content += gallery
    gallery.refreshAll
  }

  def setPreviewGallery[A](gallery : SpinnerGallery[A]) = {
    previewPane.content.clear
    previewPane.content += gallery
    gallery.refreshAll
  }

  def modules : Seq[ModuleItem] = 
    moduleRoot.children map (child => child.asInstanceOf[ModuleItem])

  def newModule(name : String) = {
    val moduleItem = new TreeItem[ModuleTreeItem] { value = ModuleItem(name) }
    moduleRoot.children += moduleItem
  }

  def buildTemplateTreeItems(node : EnvironmentNode) : TreeItem[ModuleTreeItem] = 
    node match {
      case g @ GroupNode(name) => {
        val item = 
          new TreeItem[ModuleTreeItem] {
            value = EnvironmentItem(g)
          }

        item.children ++= g.children map (buildTemplateTreeItems(_).delegate)
        item
      }
      case e @ ExpressionNode(expr) => {
        new TreeItem[ModuleTreeItem] {
          value = EnvironmentItem(e)
        }
      }
    }
  
  def addTemplateToModule(template : Template, moduleItem : TreeItem[ModuleTreeItem]) = {
    val treeItem = buildTemplateTreeItems(template.root)
    treeItem.value = TemplateItem(template)
    moduleItem.children += treeItem.delegate
  }

  def saveActiveModule(file : java.io.File) = 
    for {
      moduleItem <- activeModuleItem
    } {
      val name = moduleItem.value().asInstanceOf[ModuleItem].name
      val templates = moduleItem.children map (child => child.value().asInstanceOf[TemplateItem].template)
      val moduleXML = <module name={name}>{templates map (template => Template.templateToXML(template))}</module>
      xml.XML.save(file.getAbsolutePath, moduleXML)
    }

  def loadModule(file : java.io.File) = {
    val elem = xml.XML.loadFile(file.getAbsolutePath)

    elem match {
      case m @ <module>{templates @ _*}</module> => {
        val name = (m \ "@name").text
        val moduleItem = 
          new TreeItem[ModuleTreeItem] {
            value = ModuleItem(name)
          }

        templates foreach (t => {
          val template = Template.fromXML(t)
          addTemplateToModule(template, moduleItem)
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
