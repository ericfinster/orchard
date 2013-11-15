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
import scalafx.scene.control.TableView
import scalafx.scene.control.TableColumn
import scalafx.scene.control.TextField
import scalafx.scene.control.Label
import scalafx.scene.control.RadioButton
import scalafx.scene.control.ToggleGroup
import scalafx.scene.control.CheckBox

import scalafx.geometry.Insets
import scalafx.geometry.Orientation

import scalafx.application.JFXApp
import scalafx.application.Platform

import scalafx.collections.ObservableBuffer

import scalafx.beans.value.ObservableValue

import javafx.event.Event
import javafx.event.EventHandler

import javafx.scene.input.KeyCode
import javafx.scene.input.KeyEvent

import javafx.scene.{layout => jfxsl}
import javafx.scene.{control => jfxsc}

import javafx.beans.property.SimpleStringProperty
import javafx.beans.property.ReadOnlyStringProperty

import orchard.core._
import Util._

class EditorUI extends DialogStack(new StackPane) with EventReactor[CellEvent] {

  //============================================================================================
  // UI INITIALIZATION
  //

  getStyleClass.add("orchard-editor")

  val mainPane = new StackPane(root.asInstanceOf[jfxsl.StackPane])

  // val accordionPane = 
  //   new StackPane {
  //     padding = Insets(10, 10, 10, 10)
  //     style = "-fx-background-color: gainsboro"
  //     content = new Accordion {
  //       panes = List(
  //         new TitledPane {
  //           text = "File"
  //           content = new VBox {
  //             padding = Insets(10,10,10,10)
  //             content = List(
  //               new Button("New") { prefWidth = 100 ; onAction = newBuilder },
  //               new Button("Open") { prefWidth = 100 },
  //               new Button("Save") { prefWidth = 100 })
  //           }
  //         },
  //         new TitledPane {
  //           text = "Action"
  //           content = new VBox {
  //             padding = Insets(10,10,10,10)
  //             content = List(
  //               new Button("Assume") { prefWidth = 100 },
  //               new Button("Fill") { prefWidth = 100 },
  //               new Button("Compose") { prefWidth = 100 ; onAction = activeBuilder.emptyComposition },
  //               new Button("Extend") { prefWidth = 100 ; onAction = activeBuilder.complex.extend },
  //               new Button("Drop") { prefWidth = 100 ; onAction = activeBuilder.emptyDrop },
  //               new Button("Dump") { prefWidth = 100 ; onAction = displayMessage("Dump", "This is the dump button.") },
  //               new Button("Refresh") { prefWidth = 100 ; onAction = activeBuilder.refreshAll })
  //           }
  //         })
  //     }
  //   }

  val fileChooser = new FileChooser
  val editorPane = new TabPane

  class ExpressionWrapper(val expr : NCell[Expression]) {

    private val originStr = 
      expr.value match {
        case Variable(_, _) => "Assumed"
        case _ => "Derived"
      }

    private val thinStr =
      if (expr.value.isThin) "yes" else "no"

    val idProperty : ReadOnlyStringProperty = new SimpleStringProperty(this, "id", expr.value.id)
    val originProperty : ReadOnlyStringProperty = new SimpleStringProperty(this, "origin", originStr)
    val thinProperty : ReadOnlyStringProperty = new SimpleStringProperty(this, "thin", thinStr)

  }

  val environment = new ObservableBuffer[ExpressionWrapper]

  def envContains(id : String) : Boolean = {
    environment exists (expr => expr.idProperty.getValue == id)
  }

  val environmentTable = buildEnvironmentTable
  val previewerPane = new StackPane

  val explorerPane = new HBox {
    padding = Insets(10,10,10,10)
    spacing = 10
    content = List(environmentTable, previewerPane)
    style = "-fx-background-color: gainsboro;"
  }

  HBox.setHgrow(previewerPane, Priority.ALWAYS)

  val splitPane = new SplitPane {
    orientation = Orientation.VERTICAL
    dividerPositions = 0.5f
  }

  splitPane.getItems.addAll(editorPane, explorerPane)

  mainPane.content = splitPane

  newBuilder

  def buildEnvironmentTable : TableView[ExpressionWrapper] = {

    val table = new TableView[ExpressionWrapper] {
      prefWidth = 400
      items = environment
    }

    val idColumn = new TableColumn[ExpressionWrapper, String] { text = "Id" ; prefWidth = 100 }
    idColumn.delegate.setCellValueFactory(
      new jfxsc.cell.PropertyValueFactory[ExpressionWrapper, String]("id"))

    val originColumn = new TableColumn[ExpressionWrapper, String] { text = "Origin" }
    originColumn.delegate.setCellValueFactory(
      new jfxsc.cell.PropertyValueFactory[ExpressionWrapper, String]("origin"))

    val thinColumn = new TableColumn[ExpressionWrapper, String] { text = "Thin" }
    thinColumn.delegate.setCellValueFactory(
      new jfxsc.cell.PropertyValueFactory[ExpressionWrapper, String]("thin"))

    table.columns.addAll(idColumn, originColumn, thinColumn)

    table.selectionModel().selectedItem onChange { (_, _, e) => { setPreview(e.expr) } }

    table
  }

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

    val composeField = new TextField { promptText = "Composite" ; onAction = fillerField.requestFocus }
    val fillerField = new TextField { promptText = "Filler" ; onAction = okBtn.fire }

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

    val idField = new TextField { promptText = "Identifier" ; onAction = okBtn.fire }
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
            assumeVariable(activeBuilder.selectionBase.force, idField.text(), thinCheckBox.selected())
          }
        }
        case DialogCancel => ()
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
          onFill
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
          case KeyCode.UP => println("Up.")
          case KeyCode.DOWN => println("Down.")
          case KeyCode.F => if (ev.isControlDown) onFill  
          case KeyCode.E => if (ev.isControlDown) extrudeSelection
          case KeyCode.D => if (ev.isControlDown) extrudeDrop
          case KeyCode.C => if (ev.isControlDown) onCompose
          case KeyCode.V => if (ev.isControlDown) onAssumeVariable(ev.isShiftDown)
          case KeyCode.U => if (ev.isControlDown) onUseEnvironment
          case KeyCode.I => if (ev.isControlDown) onInsertIdentity
          case KeyCode.O => if (ev.isControlDown) onOpen
          case KeyCode.S => if (ev.isControlDown) onSave
          case _ => ()
        }
      }
    })


  def onSave = {
    fileChooser.setTitle("Save")

    val file = fileChooser.showSaveDialog(getScene.getWindow)
    // val galleryXml = cellSerializable[Polarity[String]].toXML(gallery.complex.toCell)

    // if (file != null) {
    //   xml.XML.save(file.getAbsolutePath, galleryXml)
    // }
  }

  def onOpen = {
    fileChooser.setTitle("Open")

    val file = fileChooser.showOpenDialog(getScene.getWindow)

    // if (file != null) {
    //   val elem = xml.XML.loadFile(file.getAbsolutePath)
    //   val result = cellSerializable[Polarity[String]].fromXML(elem)

    //   setGallery(new CardinalGallery[String](result))
    // }
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
      val selectedExprWrapper = environmentTable.getSelectionModel.getSelectedItem

      if (selectedExprWrapper != null) {
        fillFromEnvironment(activeBuilder.selectionBase.force, selectedExprWrapper)
      }
    }
  }

  def onAssumeVariable(thin : Boolean) = {
    if (selectionIsShell) {
      VariableDialog.thinCheckBox.selected = thin
      VariableDialog.run
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

  def assumeVariable(emptyCell : ExpressionBuilder#GalleryCell, id : String, isThin : Boolean) = {
    activeBuilder.deselectAll
    emptyCell.owner.item = Neutral(Some(Variable(id, isThin)))

    val exprCell : NCell[Expression] = emptyCell.owner.getSimpleFramework.toCell map (_.force)
    environment += new ExpressionWrapper(exprCell)

    activeBuilder.selectAsBase(emptyCell)
  }

  def fillExposedNook(nookCell : ExpressionBuilder#GalleryCell, targetId : String, fillerId : String) = {

    activeBuilder.deselectAll

    val nook = nookCell.owner.getSimpleFramework.toCell

    val (targetIsThin, targetCell) =
      if (nookCell.owner.isOutNook) {
        ((true /: (nookCell.owner.sources.force map (_.isThin))) (_&&_), nookCell.owner.target.force)
      } else {
        (nookCell.owner.target.force.isThin, nookCell.owner.emptySources.head)
      }

    targetCell.item = Neutral(Some(FillerTarget(targetId, nook, targetIsThin)))
    val tgtExprCell = targetCell.getSimpleFramework.toCell map (_.force)
    environment += new ExpressionWrapper(tgtExprCell)

    nookCell.owner.item = Neutral(Some(Filler(fillerId, nook)))
    val exprCell = nookCell.owner.getSimpleFramework.toCell map (_.force)
    environment += new ExpressionWrapper(exprCell)

  }

  def fillFromEnvironment(emptyCell : ExpressionBuilder#GalleryCell, exprWrapper : ExpressionWrapper) = {
    println("Checking compatibility ...")
    val complex = activeBuilder.complex

    emptyCell.owner.skeleton.asInstanceOf[NCell[complex.ExpressionBuilderCell]]
      .zip(exprWrapper.expr) match {
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
          println("It does!")

          activeBuilder.deselectAll

          zippedTree map (pr => {
            val (eCell, e) = pr

            // This is overkill
            eCell.item = Neutral(Some(e))
          })

          println("Transfer complete.")
        } else {
          println("Nope. Comparison failed.")
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

  def newBuilder : Unit = newBuilder(Object(None))

  def newBuilder(seed : NCell[Option[Expression]]) = {
    val builder = new ExpressionBuilder(CardinalComplex(seed))
    val newTab = new Tab { text = "Derived" ; content = builder }
    editorPane += newTab
    editorPane.selectionModel().select(newTab)
    reactTo(builder)
    builder.renderAll
  }

  def activeBuilder : ExpressionBuilder = {
    editorPane.getSelectionModel.selectedItem().content().asInstanceOf[ExpressionBuilder]
  }

  // object ExpressionSerializable extends XmlSerializable[Expression] {
  //   def toXML(expr : Expression) = 
  //     expr match {
  //       case Variable(id, isThin) => <var />
  //     }

  //   def fromXML(node : xml.Node) : Expression = Variable("x", false)
  // }

  def environmentToXML : xml.Node = {
    <environment />
  }

  def setPreview(expr : NCell[Expression]) = {
    val gallery = new FrameworkGallery(expr map (e => Some(e)))
    gallery.renderAll
    previewerPane.content = gallery
    previewerPane.requestLayout
  }
}

object Editor extends JFXApp {

  val editorUI = new EditorUI

  val orchardScene = new Scene {
      root = editorUI
    }

  orchardScene.getStylesheets.add("orchard/ui/javafx/OrchardUI.css")

  stage = new JFXApp.PrimaryStage {
      title = "Orchard"
      width = 1500
      height = 800
      scene = orchardScene
    }

}
