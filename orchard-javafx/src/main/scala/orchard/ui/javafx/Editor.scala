/**
  * Editor.scala - Main module for JavaFX Orchard Editor
  * 
  * @author Eric Finster
  * @version 0.1
  */

package orchard.ui.javafx

import scalafx.Includes._

import scalafx.scene.Scene
import scalafx.stage.FileChooser

import scalafx.scene.layout.HBox
import scalafx.scene.layout.VBox
import scalafx.scene.layout.GridPane
import scalafx.scene.layout.StackPane
import scalafx.scene.layout.AnchorPane
import scalafx.scene.layout.Priority

import scalafx.scene.control.Button
import scalafx.scene.control.Accordion
import scalafx.scene.control.TitledPane
import scalafx.scene.control.SplitPane
import scalafx.scene.control.TabPane
import scalafx.scene.control.Tab
import scalafx.scene.control.ListView
import scalafx.scene.control.ListCell
import scalafx.scene.control.TextField
import scalafx.scene.control.Label
import scalafx.scene.control.RadioButton
import scalafx.scene.control.ToggleGroup
import scalafx.scene.control.CheckBox

import scalafx.scene.image.WritableImage

import scalafx.geometry.Side
import scalafx.geometry.Insets
import scalafx.geometry.Orientation

import scalafx.application.JFXApp
import scalafx.application.Platform

import scalafx.collections.ObservableBuffer

import scalafx.beans.value.ObservableValue

import javafx.embed.swing.SwingFXUtils
import javax.imageio.ImageIO

import javafx.event.Event
import javafx.event.EventHandler

import javafx.scene.SnapshotParameters

import javafx.scene.input.KeyCode
import javafx.scene.input.KeyEvent
import javafx.scene.input.MouseEvent

import javafx.scene.{layout => jfxsl}
import javafx.scene.{control => jfxsc}

import javafx.beans.property.SimpleStringProperty
import javafx.beans.property.ReadOnlyStringProperty

import orchard.core._
import orchard.ui.javafx.controls._

import Util._

class Editor extends PopupManager(new VBox) with EventReactor[CellEvent] { thisEditor =>

  implicit val pm = thisEditor

  val fileChooser = new FileChooser

  val mainVBox = new VBox(root.delegate.asInstanceOf[jfxsl.VBox])

  val newSheetItem = new MenuItem {
    text = "New Sheet"
    onAction = newBuilder
  }

  val openItem = new MenuItem {
    text = "Open"
    onAction = onOpen
  }

  val saveItem = new MenuItem {
    text = "Save"
    onAction = onSave
  }

  val exitItem = new MenuItem {
    text = "Exit"
    onAction = javafx.application.Platform.exit
  }

  val fileMenu = new Menu {
    text = "File"
    items ++= List(newSheetItem, openItem, saveItem, exitItem)
  }

  val assumeItem = new MenuItem {
    text = "Assume Variable"
    onAction = onAssume(false)
  }

  val extrudeItem = new MenuItem {
    text = "Extrude Selection"
    onAction = extrudeSelection
  }

  val dropItem = new MenuItem {
    text = "Extrude Drop"
    onAction = extrudeDrop
  }

  val fillItem = new MenuItem {
    text = "Fill Nook"
    onAction = onFill
  }

  val composeItem = new MenuItem {
    text = "Compose Selection"
    onAction = onCompose
  }

  val identityItem = new MenuItem {
    text = "Insert Identity"
    onAction = onInsertIdentity
  }

  val useItem = new MenuItem {
    text = "Use Cell From Environment"
    onAction = onUseEnvironment
  }

  val commandMenu = new Menu {
    text = "Command"
    items ++= List(assumeItem, extrudeItem, dropItem, fillItem, composeItem, identityItem, useItem)
  }

  val helpMenu = new Menu {
    text = "Help"
    items ++= List(new MenuItem { text = "Contents" }, new MenuItem { text = "About" })
  }

  val menuBar = new MenuBar {
    menus ++= List(fileMenu, commandMenu, helpMenu)
  }

  var tabCount = 1

  val editorPane = new TabPane {
    id = "orch-editor-pane"
    side = Side.BOTTOM
    prefWidth = 1250
  }

  val environment : ObservableBuffer[NCell[Expression]] = new ObservableBuffer

  val envListView = new ListView[NCell[Expression]] { 
    items = environment 
    cellFactory = (_ => new EnvironmentCell)
  }

  class EnvironmentCell extends jfxsc.ListCell[NCell[Expression]] {

    getStyleClass add "orch-list-cell"

    var lastStyle : Option[String] = None

    def setStyleType(styleType : String) = {
      lastStyle foreach (st => getStyleClass remove st)
      getStyleClass add styleType
      lastStyle = Some(styleType)
    }

    override def updateItem(expr : NCell[Expression], empty : Boolean) = {
      super.updateItem(expr, empty)

      if (! empty) {
        // Set the style based on the semantics ...
        expr.value match {
          case Variable(_, isThin) => {
            if (isThin) {
              setStyleType("orch-list-cell-var-thin")
            } else {
              setStyleType("orch-list-cell-var")
            }
          }
          case Filler(_, _) => setStyleType("orch-list-cell-filler")
          case FillerTarget(_, _, isThin) => {
            if (isThin) {
              setStyleType("orch-list-cell-filler-tgt-thin")
            } else {
              setStyleType("orch-list-cell-filler-tgt")
            }
          }
        }

        setText(expr.toString)
      }
    }

    setOnMouseClicked(new EventHandler[MouseEvent] {
      def handle(ev : MouseEvent) {
        if (! isEmpty) {
          setPreview(getItem)

          if (ev.getClickCount > 1) {
            newBuilder(getItem map (e => Some(e)))
          }
        }
      }
    })
  }

  def envContains(id : String) : Boolean = {
    environment exists (expr => expr.value.id == id)
  }

  val previewerPane = new StackPane {
    id = "orch-previewer-pane"
  }

  val environmentPane = new AnchorPane {
    id = "orch-environment-pane"
    prefWidth = 250
  }

  AnchorPane.setTopAnchor(envListView, 10)
  AnchorPane.setLeftAnchor(envListView, 10)
  AnchorPane.setBottomAnchor(envListView, 10)
  AnchorPane.setRightAnchor(envListView, 10)

  environmentPane.content.addAll(envListView)

  val verticalSplit = new SplitPane {
    orientation = Orientation.VERTICAL
    dividerPositions = 0.8f
  }

  verticalSplit.items.addAll(editorPane, previewerPane)

  val horizontalSplit = new SplitPane {
    orientation = Orientation.HORIZONTAL
    dividerPositions = 0.2f
  }

  horizontalSplit.items.addAll(environmentPane, verticalSplit)

  VBox.setVgrow(horizontalSplit, Priority.ALWAYS)
  mainVBox.content.addAll(menuBar, horizontalSplit)

  newBuilder

  //============================================================================================
  // DIALOG DEFINITIONS
  //

  object EditorMessage extends Dialog {

    val message = new Label

    borderPane.center = new StackPane { padding = Insets(10,10,10,10) ; content = message }

    def setTitle(title : String) = heading.text = title
    def setMessage(msg : String) = message.text = msg

    def onHide = ()
    def onShow = ()

  }

  def displayMessage(title : String, message : String) = {
    EditorMessage.setTitle(title)
    EditorMessage.setMessage(message)
    EditorMessage.run
  }

  abstract class ComposeInfoDialog extends CancellableDialog {

    val composeField = new TextField { promptText = "Composite" ; onAction = () => { fillerField.requestFocus } }
    val fillerField = new TextField { promptText = "Filler" ; onAction = () => { okBtn.fire } }

    borderPane.center = 
      new VBox {
        padding = Insets(10,10,10,10)
        spacing = 10
        content = List(composeField, fillerField)
      }

  }

  class FillNookDialog(nookCell : ExpressionBuilder#GalleryCell) extends ComposeInfoDialog {

    heading.text = "Fill Nook"

    def onShow = {
      composeField.clear
      fillerField.clear
      composeField.requestFocus
    }

    def onHide =
      response match {
        case DialogOK => {

          val compositeId = composeField.text()
          val fillerId = fillerField.text()

          if (envContains(compositeId) || envContains(fillerId)) {
            println("Error: Duplicate Identifier")
          } else {
            fillExposedNook(nookCell, composeField.text(), fillerField.text())
          }
        }
        case DialogCancel => ()
      }
  }

  class IdentityDialog(expr : Expression) extends ComposeInfoDialog {

    heading.text = "Insert Identity"

    composeField.text = "id-" ++ expr.id
    fillerField.text = "univ-" ++ expr.id

    def onShow = composeField.requestFocus

    def onHide = 
      response match {
        case DialogOK => {
          extrudeDrop

          val compositeId = composeField.text()
          val fillerId = fillerField.text()

          if (envContains(compositeId) || envContains(fillerId)) {
            println("Error: Duplicate Identifier")
          } else {
            fillExposedNook(activeBuilder.lastFiller, composeField.text(), fillerField.text())
          }
        }
        case DialogCancel => ()
      }

  }

  object ComposeDialog extends ComposeInfoDialog {

    heading.text = "Insert Composite"

    def onShow = {
      composeField.clear()
      fillerField.clear()
      composeField.requestFocus
    }

    def onHide = 
      response match {
        case DialogOK => {
          extrudeSelection

          val compositeId = composeField.text()
          val fillerId = fillerField.text()

          if (envContains(compositeId) || envContains(fillerId)) {
            println("Error: Duplicate Identifier")
          } else {
            fillExposedNook(activeBuilder.lastFiller, composeField.text(), fillerField.text())
          }
        }
        case DialogCancel => ()
      }
  }

  object VariableDialog extends Dialog {

    heading.text = "Assume Variable"

    val idField = new TextField { promptText = "Identifier" ; onAction = () => { okBtn.fire } }
    val thinCheckBox = new CheckBox("Thin") { allowIndeterminate = false }

    borderPane.center = 
      new VBox {
        padding = Insets(10,10,10,10)
        spacing = 10
        content = List(idField, thinCheckBox)
      }

    def onShow = {
      idField.clear
      idField.requestFocus
    }

    def onHide = 
      response match {
        case DialogOK => {
          if (envContains(idField.text())) {
            println("Error: Duplicate Identifier")
          } else {
            assume(activeBuilder.selectionBase.force, idField.text(), thinCheckBox.selected())
          }
        }
        case DialogCancel => ()
      }

  }

  object ViewerDialog extends Dialog {

    heading.text = "View Expression"

    val viewerArea = new StackPane { padding = Insets(10,10,10,10) }
    borderPane.center = viewerArea

    def onHide = ()
    def onShow = ()

    addEventFilter(KeyEvent.KEY_PRESSED,
      new EventHandler[KeyEvent] {
        def handle(ev : KeyEvent) {
          ev.getCode match {
            case KeyCode.X => if (ev.isControlDown) onWrite
            case _ => ()
          }
        }
      })

    def onWrite = {
      fileChooser.setTitle("Export Snapshot")

      val file = fileChooser.showSaveDialog(getScene.getWindow)

      if (file != null) {

        val image = viewerArea.content.head.snapshot(new SnapshotParameters, null)

        try {
          ImageIO.write(SwingFXUtils.fromFXImage(image, null), "png", file)
        } catch {
          case e : java.io.IOException => {
            println("There was an error writing to the file!.")
          }
        }
      }
    }
  }

  //============================================================================================
  // EVENTS
  //

  def onEventEmitted(ev : CellEvent) = 
    ev match {
      case CellDoubleClicked(c) => {
        val cell = c.asInstanceOf[ExpressionBuilder#GalleryCell]

        if (cell.owner.isEmpty) {
          //onFill
          ()
        } else {
          // Let's open the cell in a new tab here
          val theCell : NCell[Option[Expression]] = cell.owner.getSimpleFramework.toCell
          newBuilder(theCell)
        }
      }

      case _ => ()
    }

  addEventFilter(KeyEvent.KEY_PRESSED,
    new EventHandler[KeyEvent] {
      def handle(ev : KeyEvent) {
        ev.getCode match {
          case KeyCode.LEFT => activeBuilder.prev
          case KeyCode.RIGHT => activeBuilder.next
          case KeyCode.F => if (ev.isControlDown) onFill  
          case KeyCode.E => if (ev.isControlDown) extrudeSelection
          case KeyCode.D => if (ev.isControlDown) extrudeDrop
          case KeyCode.C => if (ev.isControlDown) onCompose
          case KeyCode.A => if (ev.isControlDown) onAssume(ev.isShiftDown)
          case KeyCode.U => if (ev.isControlDown) onUseEnvironment
          case KeyCode.V => if (ev.isControlDown) onView
          case KeyCode.I => if (ev.isControlDown) onInsertIdentity
          case KeyCode.O => if (ev.isControlDown) onOpen
          case KeyCode.S => if (ev.isControlDown) onSave
          case KeyCode.N => if (ev.isControlDown) onNew
          case KeyCode.L => if (ev.isControlDown) onLoadExpr
          case KeyCode.G => if (ev.isControlDown) onGlobCardinal
          case KeyCode.X => if (ev.isControlDown) onExtra
          case KeyCode.P => if (ev.isControlDown) onPrintScreen
          case KeyCode.M => if (ev.isControlDown) displayMessage("Message", "This is a message!")
          case KeyCode.Z => if (ev.isControlDown) { debug = ! debug ; println("Debug is now: " ++ (if (debug) "on" else "off")) }
          case _ => ()
        }
      }
    })

  def onAssume(thin : Boolean) = {
    if (selectionIsShell) {
      VariableDialog.thinCheckBox.selected = thin
      VariableDialog.run
    }
  }

  def onView = {
    activeBuilder.selectionBase foreach (cell => {
      val selectedExpr = cell.owner.getSimpleFramework.toCell
      val gallery = new StaticFrameworkGallery(selectedExpr)
      ViewerDialog.viewerArea.content = gallery
      gallery.renderAll
      ViewerDialog.run
    })
  }

  def onPrintScreen = {
    fileChooser.setTitle("Export Snapshot")

    val file = fileChooser.showSaveDialog(getScene.getWindow)

    if (file != null) {

      val image = activeBuilder.snapshot(null, null)

      try {
        ImageIO.write(SwingFXUtils.fromFXImage(image, null), "png", file)
      } catch {
        case e : java.io.IOException => {
          println("There was an error writing to the file!.")
        }
      }
    }
  }

  def onLoadExpr = {
    val selectedExpr = envListView.getSelectionModel.getSelectedItem

    if (selectedExpr != null) {
      newBuilder(selectedExpr map (e => Some(e)))
    }
  }

  def onExtra = {
  }

  def onGlobCardinal = {
    activeBuilder.complex.extend
  }

  def onNew = {
    emptyEditor
    newBuilder
  }

  def onSave = {
    fileChooser.setTitle("Save")

    val file = fileChooser.showSaveDialog(getScene.getWindow)

    if (file != null) {
      saveEditorState(file)
    }
  }

  def onOpen = {
    fileChooser.setTitle("Open")

    val file = fileChooser.showOpenDialog(getScene.getWindow)

    if (file != null) {
      loadEditorState(file)
    }
  }

  def onCompose = {
    if (selectionIsComposable) ComposeDialog.run
  }

  // The fact that these are multiplying means you need a routine for it.
  // You should pass in a function that works on ? only if the thing satisfies
  // this condition ...
  def onInsertIdentity = {
    if (selectionIsComposable) {
      val cell = activeBuilder.selectionBase.force
      cell.item match {
        case Neutral(Some(expr)) => {
          val idDialog = new IdentityDialog(expr)
          idDialog.run
        }
        case _ => ()
      }
    }
  }

  def onUseEnvironment = {
    if (selectionIsEmptyCell) {
      val selectedExpr = envListView.getSelectionModel.getSelectedItem

      if (selectedExpr != null) {
        fillFromEnvironment(activeBuilder.selectionBase.force, selectedExpr)
      }
    }
  }

  def onFill = {
    activeBuilder.selectionBase match {
      case None => ()
      case Some(cell) => {
        if (cell.owner.isExposedNook) {
          val fillDialog = new FillNookDialog(cell)
          fillDialog.run
        }
      }
    }
  }

  //============================================================================================
  // EDITOR SEMANTICS
  //

  def selectionIsComposable : Boolean = {
    val cellsAreComplete = (true /: (activeBuilder.selectedCells map (_.owner.isComplete))) (_&&_)
    cellsAreComplete && selectionIsExtrudable
  }

  def selectionIsShell : Boolean = {
    activeBuilder.selectionBase match {
      case None => false
      case Some(cell) => cell.owner.isShell
    }
  }

  def selectionIsEmptyCell : Boolean = {
    activeBuilder.selectionBase match {
      case None => false
      case Some(cell) => cell.owner.isEmpty
    }
  }

  def selectionIsExtrudable : Boolean = {
    activeBuilder.selectionBase match {
      case None => false
      case Some(cell) => {
        cell.container match {
          case None => false
          case Some(cont) => cont.owner.isPolarized
        }
      }
    }
  }

  def extrudeSelection = {
    // Add new empty cells based on the current selection
    if (selectionIsExtrudable) {
      activeBuilder.emptyComposition
      activeBuilder.clearAndSelect(activeBuilder.lastComposite)
    }
  }

  def extrudeDrop = {
    activeBuilder.selectionBase match {
      case None => ()
      case Some(cell) => {
        cell.container match {
          case None => ()
          case Some(cont) => {
            if (cont.owner.isPolarized) {
              activeBuilder.emptyDrop
              activeBuilder.clearAndSelect(cell)
            }
          }
        }
      }
    }
  }

  def fillExposedNook(nookCell : ExpressionBuilder#GalleryCell, targetId : String, fillerId : String) = {

    activeBuilder.deselectAll

    val nook = nookCell.owner.getSimpleFramework.getIdNook

    val (targetIsThin, targetCell) =
      if (nookCell.owner.isOutNook) {
        ((true /: (nookCell.owner.sources.force map (_.isThin))) (_&&_), nookCell.owner.target.force)
      } else {
        (nookCell.owner.target.force.isThin, nookCell.owner.emptySources.head)
      }

    targetCell.item = Neutral(Some(FillerTarget(targetId, nook, targetIsThin)))
    val tgtExprCell = targetCell.getSimpleFramework.toCell map (_.force)
    environment += tgtExprCell

    nookCell.owner.item = Neutral(Some(Filler(fillerId, nook)))
    val exprCell = nookCell.owner.getSimpleFramework.toCell map (_.force)
    environment += exprCell

  }

  def fillFromEnvironment(emptyCell : ExpressionBuilder#GalleryCell, expr : NCell[Expression]) = {
    val complex = activeBuilder.complex

    emptyCell.owner.skeleton.asInstanceOf[NCell[complex.ExpressionBuilderCell]]
      .zip(expr) match {
      case None => println("Not compatible. Zip failed.")
      case Some(zippedTree) => {

        var itFits = true

        zippedTree map (pr => {
          val (eCell, e) = pr

          eCell.item match {
            case Neutral(None) => ()
            case Neutral(Some(f)) => if (itFits) { itFits &&= (e == f) } else ()
            case _ => itFits = false
          }
        })

        if (itFits) {
          activeBuilder.deselectAll

          zippedTree map (pr => {
            val (eCell, e) = pr

            // This is overkill
            eCell.item = Neutral(Some(e))
          })
        } else {
          println("Cell does not fit.")
        }
      }
    }
  }

  def newBuilder : Unit = newBuilder(Object(None))

  def newBuilder(seed : NCell[Option[Expression]]) = {
    val builder = new ExpressionBuilder(CardinalComplex(seed))
    val newTab = new Tab { text = "Sheet " ++ tabCount.toString ; content = builder }
    tabCount += 1
    editorPane += newTab
    editorPane.selectionModel().select(newTab)
    reactTo(builder)
    builder.renderAll
  }

  def activeBuilder : ExpressionBuilder = {
    editorPane.getSelectionModel.selectedItem().content().asInstanceOf[ExpressionBuilder]
  }

  def setPreview(expr : NCell[Expression]) = {
    val gallery = new FrameworkGallery(expr map (e => Some(e)))
    gallery.renderAll
    previewerPane.content = gallery
    previewerPane.requestLayout
  }

  def assume(emptyCell : ExpressionBuilder#GalleryCell, id : String, isThin : Boolean) = {
    activeBuilder.deselectAll
    emptyCell.owner.item = Neutral(Some(Variable(id, isThin)))

    val exprCell : NCell[Expression] = emptyCell.owner.getSimpleFramework.toCell map (_.force)
    environment += exprCell

    activeBuilder.selectAsBase(emptyCell)
  }

  def emptyEditor = {
    tabCount = 1
    editorPane.tabs.clear
    environment.clear
    previewerPane.content.clear
  }

  def environmentFromXML(node : xml.Node) = {
    import XmlSerializable._

    node match {
      case <environment>{cells @ _*}</environment> => {
        trimText(cells) foreach (cell => {
          val res : NCell[Expression] = cellSerializable[Expression].fromXML(cell)
          environment += res
        })
      }
    }
  }

  def environmentToXML : xml.Node = {
    import XmlSerializable._

    <environment>{
      environment map (expr => cellSerializable[Expression].toXML(expr))
    }</environment>
  }

  def saveEditorState(file : java.io.File) = {
    import XmlSerializable._

    val tabCells : List[NCell[Polarity[Option[Expression]]]] = 
      (editorPane.tabs map (t => t.content().asInstanceOf[ExpressionBuilder].complex.toCell)).toList

    // Again, this is wildly inefficient.  The serializations should use the environment.
    val stateXML = <editor><views>{
      tabCells map (cell => cellSerializable[Polarity[Option[Expression]]].toXML(cell))
    }</views>{environmentToXML}</editor>

    xml.XML.save(file.getAbsolutePath, stateXML)
  }

  def loadEditorState(file : java.io.File) = {
    import XmlSerializable._

    emptyEditor

    val elem = xml.XML.loadFile(file.getAbsolutePath)

    elem match {
      case <editor><views>{viewCells @ _*}</views>{env}</editor> => {
        environmentFromXML(env)

        trimText(viewCells) foreach (view => {
          val viewExpr = cellSerializable[Polarity[Option[Expression]]].fromXML(view)
          val builder = new ExpressionBuilder(viewExpr)
          val newTab = new Tab { text = "Sheet " ++ tabCount.toString ; content = builder }
          tabCount += 1
          editorPane += newTab
          reactTo(builder)
          builder.renderAll
        })
      }
    }
  }
}

object Editor extends JFXApp {

  val orchardScene = new Scene {
    root = new Editor
  }

  orchardScene.getStylesheets.add("orchard/ui/javafx/OrchardUI.css")

  stage = new JFXApp.PrimaryStage {
      title = "Orchard"
      width = 1500
      height = 800
      scene = orchardScene
    }

}
