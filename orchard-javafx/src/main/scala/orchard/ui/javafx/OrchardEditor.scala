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

  sealed trait TemplateTreeItem
  case class TemplateItem(template : Template) extends TemplateTreeItem { override def toString = template.name }
  case class EnvironmentItem(node : EnvironmentNode) extends TemplateTreeItem

  class TemplateTreeCell extends jfxsc.TreeCell[TemplateTreeItem] {

    getStyleClass add "orch-list-cell"
    val cellStyleIndex = getStyleClass.length
    getStyleClass add "orch-list-null"

    def setCellStyleType(str : String) = {
      getStyleClass(cellStyleIndex) = str
    }

    override def updateItem(item : TemplateTreeItem, empty : Boolean) = {
      super.updateItem(item, empty)

      if (! empty) {
        item match {
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

  val templateTreeRoot = new TreeItem[TemplateTreeItem]
  val templateTreeView = 
    new TreeView[TemplateTreeItem] {
      root = templateTreeRoot
      showRoot = false
      cellFactory = (_ => new TemplateTreeCell)
    }

  templateTreeView.selectionModel().selectedItem onChange {
    val item = templateTreeView.selectionModel().getSelectedItem
    
    if (item != null) {
      item.value() match {
        case EnvironmentItem(ExpressionNode(expr)) => setPreview(expr)
        case _ => ()
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

  val templatePane = new TitledPane {
    text = "Templates"
    content = templateTreeView
    collapsible = false
  }

  val templateAnchor = new AnchorPane {
    content = templatePane
    styleClass += "orch-pane"
  }

  AnchorPane.setTopAnchor(templatePane, 10)
  AnchorPane.setRightAnchor(templatePane, 10)
  AnchorPane.setBottomAnchor(templatePane, 10)
  AnchorPane.setLeftAnchor(templatePane, 10)

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
    items.addAll(workspaceAnchor, templateAnchor)
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
          case KeyCode.O => if (ev.isControlDown) onOpen
          case KeyCode.S => if (ev.isControlDown) onSave
  //         // case KeyCode.V => if (ev.isControlDown) onView
          case KeyCode.B => if (ev.isControlDown) onSubstitute
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

  def onExtrude : Unit = for { wksht <- activeSheet } { wksht.extrude }
  def onDrop : Unit = for { wksht <- activeSheet } { wksht.drop }

  def onAssume(thinHint : Boolean) : Unit = for { wksp <- activeWorkspace } { wksp.assumeAtSelection(thinHint) }
  def onFill : Unit = for { wksp <- activeWorkspace } { wksp.fillAtSelection }
  def onUseEnvironment : Unit = for { wksp <- activeWorkspace } { wksp.expressionToSelection }

  def onNewWorkspace = NewWorkspaceDialog.run
  def onNewSheet = for { wksp <- activeWorkspace } { wksp.newSheet }

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

  def onNewTemplate = 
    for {
      wksp <- activeWorkspace
    } {
      println("Exporting workspace as template ...")
      addTemplate(wksp.templateSnapshot)
    }

  def onApplyTemplate = 
    for {
      wksp <- activeWorkspace
      template <- activeTemplate
    } {
      println("Applying template")
      wksp.importTemplate(template)
    }

  def onApplyTemplateInShell = 
    for {
      wksp <- activeWorkspace
      template <- activeTemplate
    } {
      wksp.importTemplateAtSelection(template)
    }

  def onOpen = {
    fileChooser.setTitle("Open")

    val file = fileChooser.showOpenDialog(getScene.getWindow)

    if (file != null) {
      loadTemplates(file)
    }
  }

  def onSave = {
    fileChooser.setTitle("Save")

    val file = fileChooser.showSaveDialog(getScene.getWindow)

    if (file != null) {
      saveTemplates(file)
    }
  }

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

  def activeSheet : Option[JavaFXWorkspace#Worksheet] = 
    for { 
      wksp <- activeWorkspace 
      sheet <- wksp.activeSheet
    } yield sheet

  def activeTemplate : Option[Template] = {
    val item = templateTreeView.selectionModel().getSelectedItem

    if (item != null) {
      var curItem = item

      while (curItem.parent() != templateTreeRoot.delegate) {
        curItem = curItem.parent()
      }

      Some(curItem.value().asInstanceOf[TemplateItem].template)
    } else
      None
  }

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

  def closeActiveWorkspace =
    for { wksp <- activeWorkspace } { closeWorkspace(wksp) }

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

  def buildTemplateTreeItems(node : EnvironmentNode) : TreeItem[TemplateTreeItem] = 
    node match {
      case g @ GroupNode(name) => {
        val item = 
          new TreeItem[TemplateTreeItem] {
            value = EnvironmentItem(g)
          }

        item.children ++= g.children map (buildTemplateTreeItems(_).delegate)
        item
      }
      case e @ ExpressionNode(expr) => {
        new TreeItem[TemplateTreeItem] {
          value = EnvironmentItem(e)
        }
      }
    }
  
  def templates : Seq[Template] = {
    templateTreeRoot.children map (child =>
      child.value() match {
        case TemplateItem(template) => template
        case _ => ???
      }
    )
  }

  def addTemplate(template : Template) = {
    val treeItem = buildTemplateTreeItems(template.root)
    treeItem.value = TemplateItem(template)
    templateTreeRoot.children += treeItem.delegate
  }

  def saveTemplates(file : java.io.File) = {
    val moduleXML = <module>{templates map (template => Template.templateToXML(template))}</module>
    xml.XML.save(file.getAbsolutePath, moduleXML)
  }

  def loadTemplates(file : java.io.File) = {
    // clearTemplates

    val elem = xml.XML.loadFile(file.getAbsolutePath)

    elem match {
      case <module>{templates @ _*}</module> => {
        templates foreach (t => {
          val template = Template.fromXML(t)
          addTemplate(template)
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
