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

import scalafx.scene.layout._
import scalafx.geometry._

import scalafx.scene.control.TextField
import scalafx.scene.control.Button
import scalafx.scene.control.SplitPane
import scalafx.scene.control.ListView
import scalafx.scene.control.TabPane
import scalafx.scene.control.TitledPane
import scalafx.scene.control.CheckBox
import scalafx.scene.control.TreeView
import scalafx.scene.control.TreeItem
import scalafx.scene.control.Accordion
import scalafx.scene.control.Tab

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

import orchard.core._
import orchard.ui.javafx.controls._

class Editor extends PopupManager(new VBox) { thisEditor =>

  implicit val pm = thisEditor

  val mainVBox = new VBox(root.delegate.asInstanceOf[jfxsl.VBox])

  val exitItem = new MenuItem {
    text = "Exit"
    onAction = javafx.application.Platform.exit
  }

  val fileMenu = new Menu {
    text = "File"
    items ++= List(exitItem)
  }

  val fromCell = new MenuItem {
    text = "New From Cell"
  }

  val fromEnvironment = new MenuItem {
    text = "New From Environment"
    onAction = onDefnFromEnv
  }

  val definitionMenu = new Menu {
    text = "Definition"
    items ++= List(fromCell, fromEnvironment)
  }

  val extrudeItem = new MenuItem {
    text = "Extrude Selection"
    onAction = onExtrude
  }

  val dropItem = new MenuItem {
    text = "Extrude Drop"
    onAction = onDrop
  }

  val assumeItem = new MenuItem {
    text = "Assume Variable"
    onAction = onAssume(false)
  }

  val composeItem = new MenuItem {
    text = "Compose Selection"
    onAction = onCompose
  }

  val identityItem = new MenuItem {
    text = "Insert Identity"
    onAction = onInsertIdentity
  }

  val fillItem = new MenuItem {
    text = "Fill Nook"
    onAction = onFill
  }

  val useItem = new MenuItem {
    text = "Use Cell From Environment"
    onAction = onUseEnvironment
  }

  val expressionMenu = new Menu {
    text = "Expression"
    items ++= List(extrudeItem, dropItem, assumeItem, composeItem, identityItem, fillItem, useItem)
  }

  val menuBar = new MenuBar {
    menus ++= List(fileMenu, definitionMenu, expressionMenu)
  }

  val sheetTabPane = new TabPane {
    side = Side.TOP
  }

  val sheetPane = new StackPane {
    content = sheetTabPane
    padding = Insets(10,10,10,10)
    styleClass += "orch-pane"
  }

  val environment = ObservableBuffer.empty[NCell[Expression]]

  val environmentListView = 
    new ListView[NCell[Expression]] {
      items = environment
      cellFactory = (_ =>
        new EnvironmentCell {
          setOnMouseClicked(new EventHandler[MouseEvent] {
            def handle(ev : MouseEvent) {
              if (! isEmpty) {
                setPreview(getItem)

                if (ev.getClickCount > 1) {
                  newSheet(getItem map (e => Some(e)))
                }
              }
            }
          })
        })
    }

  val environmentPane = new TitledPane {
    text = "Environment"
    content = environmentListView
  }

  val substitutionsPane = new TitledPane {
    text = "Substitutions"
  }

  sealed trait DefinitionTreeItem
  case class DefinitionItem(defn : Definition) extends DefinitionTreeItem { override def toString = defn.toString }
  case object HypothesesItem extends DefinitionTreeItem { override def toString = "Hypotheses" }
  case object ConclusionItem extends DefinitionTreeItem { override def toString = "Conclusions" }
  case class ExpressionItem(expr : NCell[Expression]) extends DefinitionTreeItem { override def toString = expr.value.id }

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
              case Filler(_, _) => setStyleType("orch-list-cell-filler")
              case FillerFace(_, _, isThin) => {
                if (isThin) {
                  setStyleType("orch-list-cell-filler-face-thin")
                } else {
                  setStyleType("orch-list-cell-filler-face")
                }
              }
            }

            setText(expr.toString)
          }
          case item @ _ => { clearStyleType ; setText(item.toString) }
        }
      }
    }

    setOnMouseClicked(new EventHandler[MouseEvent] {
      def handle(ev : MouseEvent) {
        if (! isEmpty) {
          getItem match {
            case ExpressionItem(expr) => setPreview(expr)
            case _ => ()
          }

          if (ev.getClickCount > 1) {
            // newSheet(newCell.getItem map (e => Some(e)))
          }
        }
      }
    })

  }

  val definitionTreeRoot = new TreeItem[DefinitionTreeItem]
  val definitionTreeView = 
    new TreeView[DefinitionTreeItem] {
      root = definitionTreeRoot
      showRoot = false
      cellFactory = (_ => new DefinitionTreeCell)
    }

  val definitionsPane = new TitledPane {
    text = "Definitions"
    content = definitionTreeView
  }

  val accordion = new Accordion {
    panes = List(environmentPane, definitionsPane)
    expandedPane = environmentPane
  }

  val accordionPane = new AnchorPane {
    content = accordion
    styleClass += "orch-pane"
  }

  AnchorPane.setTopAnchor(accordion, 10)
  AnchorPane.setRightAnchor(accordion, 10)
  AnchorPane.setBottomAnchor(accordion, 10)
  AnchorPane.setLeftAnchor(accordion, 10)

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
    items.addAll(accordionPane, verticalSplit)
    dividerPositions = 0.1f
  }

  VBox.setVgrow(horizontalSplit, Priority.ALWAYS)
  mainVBox.content.addAll(menuBar, horizontalSplit)

  //============================================================================================
  // DIALOG DEFINITIONS
  //

  abstract class FillingDialog(freeVars : Seq[NCell[Expression]]) extends CancellableDialog {

    val freeVarList = new ListView[NCell[Expression]] {
      items = ObservableBuffer(freeVars)
      cellFactory = (_ => {
        val newCell = new EnvironmentCell
        newCell
      })
    }

    val composeField = new TextField { promptText = "Composite" ; onAction = () => { fillerField.requestFocus } }
    val fillerField = new TextField { promptText = "Filler" ; onAction = () => { okBtn.fire } }

    borderPane.center = 
      new VBox {
        padding = Insets(10,10,10,10)
        spacing = 10
        content = List(freeVarList, composeField, fillerField)
      }

    def onShow = {
      composeField.clear
      fillerField.clear
      composeField.requestFocus
    }

    def parseResults : Option[(List[IdentToken], List[IdentToken])] = {

      import IdentParser.Success
      import IdentParser.NoSuccess

      IdentParser(composeField.text()) match {
        case Success(composeIdent, _) => {
          IdentParser(fillerField.text()) match {
            case Success(fillerIdent, _) => {

              def validRef(ident : IdentToken) : Boolean = 
                ident match {
                  case LiteralToken(_) => true
                  case ReferenceToken(id) => freeVars exists (expr => expr.value.id == id)
                }

              val validRefs = (true /: ((composeIdent ++ fillerIdent) map (validRef(_)))) (_&&_)

              if (validRefs) {
                // BUG: Does not check that the two are not given the *same* name ... and
                // BUG: We shouldn't allow the empty string.

                if (environmentContains(IdentToken.getId(composeIdent)) ||
                    environmentContains(IdentToken.getId(fillerIdent))) { println("Duplicate identifier.") ; None }
                else Some(composeIdent, fillerIdent)
              } else { println("Missing a variable.") ; None }
            }
            case _ : NoSuccess => { println("Filler parse failed.") ; None }
          }
        }
        case _ : NoSuccess => { println("Compose parse failed.") ; None }
      }
    }
  }

  class FillNookDialog(nookCell : ExpressionBuilder#GalleryCell) 
      extends FillingDialog(nookCell.owner.getSimpleFramework.freeVariables.values.toSeq) {

    heading.text = "Fill Nook"

    def onHide =
      response match {
        case DialogOK =>
          for {
            exprBuilder <- expressionBuilder
            (composeIdent, fillerIdent) <- parseResults
          } {
            fillExposedNook(nookCell, composeIdent, fillerIdent)
          }
        case DialogCancel => ()
      }

  }

  class IdentityDialog(freeVars : Seq[NCell[Expression]], expr : Expression) extends FillingDialog(freeVars) {

    heading.text = "Insert Identity"

    composeField.text = "id-${" ++ expr.id ++ "}"
    fillerField.text = "def-id-${" ++ expr.id ++ "}"

    override def onShow = { composeField.requestFocus }

    def onHide =
      response match {
        case DialogOK => 
          for {
            exprBuilder <- expressionBuilder
            (composeIdent, fillerIdent) <- parseResults
          } {
            exprBuilder.extrudeDrop
            fillExposedNook(exprBuilder.lastFiller, composeIdent, fillerIdent)
          }
        case DialogCancel => ()
      }

  }

  class ComposeDialog(freeVars : Seq[NCell[Expression]]) extends FillingDialog(freeVars) {

    heading.text = "Insert Composite"

    def onHide = 
      response match {
        case DialogOK => ()
          for {
            exprBuilder <- expressionBuilder
            (composeIdent, fillerIdent) <- parseResults
          } {
            exprBuilder.extrudeSelection
            fillExposedNook(exprBuilder.lastFiller, composeIdent, fillerIdent)
          }
        case DialogCancel => ()
      }
  }

  object VariableDialog extends CancellableDialog {

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
          if (environmentContains(idField.text())) {
            println("Error: Duplicate Identifier")
          } else {
            assume(idField.text(), thinCheckBox.selected())
          }
        }
        case DialogCancel => ()
      }

  }

  object DefinitionDialog extends Dialog {

    heading.text = "New Definition"

    val nameField = new TextField { promptText = "Definition name" }

    borderPane.center = 
      new VBox {
        padding = Insets(10,10,10,10)
        spacing = 10
        content = List(nameField)
      }

    def onShow = {
      nameField.requestFocus
    }

    def onHide = {
      newDefnFromEnv(nameField.text())
      nameField.clear
    }

  }

  //============================================================================================
  // EVENTS
  //

  addEventFilter(KeyEvent.KEY_PRESSED,
    new EventHandler[KeyEvent] {
      def handle(ev : KeyEvent) {
        ev.getCode match {
          case KeyCode.LEFT => for { exprBuilder <- expressionBuilder } exprBuilder.prev 
          case KeyCode.RIGHT => for { exprBuilder <- expressionBuilder } exprBuilder.next
          case KeyCode.E => if (ev.isControlDown) onExtrude
          case KeyCode.D => if (ev.isControlDown) onDrop
          case KeyCode.A => if (ev.isControlDown) onAssume(ev.isShiftDown)
          case KeyCode.C => if (ev.isControlDown) onCompose
          case KeyCode.I => if (ev.isControlDown) onInsertIdentity
          case KeyCode.F => if (ev.isControlDown) onFill  
          case KeyCode.U => if (ev.isControlDown) onUseEnvironment
          case KeyCode.T => if (ev.isControlDown) newSheet
          // case KeyCode.V => if (ev.isControlDown) onView
          // case KeyCode.O => if (ev.isControlDown) onOpen
          // case KeyCode.S => if (ev.isControlDown) onSave
          // case KeyCode.N => if (ev.isControlDown) onNewSheet
          // case KeyCode.L => if (ev.isControlDown) onLoadExpr
          // case KeyCode.G => if (ev.isControlDown) onGlobCardinal
          // case KeyCode.X => if (ev.isControlDown) onExtra
          // case KeyCode.P => if (ev.isControlDown) onPrintScreen
          // case KeyCode.W => if (ev.isControlDown) onWebView
          // case KeyCode.M => if (ev.isControlDown) displayMessage("Message", "This is a message!")
          // case KeyCode.Z => if (ev.isControlDown) { debug = ! debug ; println("Debug is now: " ++ (if (debug) "on" else "off")) }
          case _ => ()
        }
      }
    })

  def onExtrude = {
    for { exprBuilder <- expressionBuilder } {
      exprBuilder.extrudeSelection
    }
  }

  def onDrop = {
    for { exprBuilder <- expressionBuilder } {
      exprBuilder.extrudeDrop
    }
  }

  def onAssume(thin : Boolean) = {
    for { 
      exprBuilder <- expressionBuilder
      cell <- exprBuilder.selectionBase
    } {
      if (cell.owner.isShell) {
        VariableDialog.thinCheckBox.selected = thin
        VariableDialog.run
      } else {
        println("Error: selection is not a shell!")
      }
    }
  }

  def onCompose = {
    for { exprBuilder <- expressionBuilder } {
      if (exprBuilder.selectionIsComposable) {
        val freeVars = HashMap.empty[String, NCell[Expression]]

        exprBuilder.selectedCells foreach (cell => {
          cell.owner.getSimpleFramework.collectDependencies(freeVars)
        })

        freeVars filter (pr => {
          val (id, expr) = pr
          expr.value match {
            case Variable(_, _) => true
            case _ => false
          }
        })
        
        new ComposeDialog(freeVars.values.toSeq).run
      }
    }
  }

  def onInsertIdentity = {
    for { exprBuilder <- expressionBuilder } {
      if (exprBuilder.selectionIsComposable) {
        for { cell <- exprBuilder.selectionBase } {
          cell.item match {
            case Neutral(Some(expr)) => {
              val freeVars = cell.owner.getSimpleFramework.freeVariables.values.toSeq
              new IdentityDialog(freeVars, expr).run
            }
            case _ => ()
          }
        }
      }
    }
  }

  def onFill = {
    for {
      exprBuilder <- expressionBuilder
      selectedCell <- exprBuilder.selectionBase
    } {
      if (selectedCell.owner.isExposedNook) {
        new FillNookDialog(selectedCell).run
      }
    }
  }

  def onUseEnvironment = {
    for { 
      exprBuilder <- expressionBuilder
      selectedCell <- exprBuilder.selectionBase
    } {
      if (selectedCell.owner.isEmpty) {
        val selectedExpr = environmentListView.getSelectionModel.getSelectedItem

        if (selectedExpr != null) {
          fillFromEnvironment(selectedCell, selectedExpr)
        }
      }
    }
  }

  def onDefnFromEnv = {
    DefinitionDialog.run
  }

  //============================================================================================
  // SEMANTIC ROUTINES
  //

  def environmentContains(id : String) : Boolean = {
    environment exists (expr => expr.value.id == id)
  }

  def assume(id : String, isThin : Boolean) = {
    for { 
      exprBuilder <- expressionBuilder
      emptyCell <- exprBuilder.selectionBase
    } {
      if (emptyCell.owner.isShell) {
        exprBuilder.deselectAll
        emptyCell.owner.item = Neutral(Some(Variable(id, isThin)))

        // To update the highlighting ...
        exprBuilder.refreshAll

        val exprCell : NCell[Expression] = emptyCell.owner.getSimpleFramework.toCell map (_.get)
        environment += exprCell

        exprBuilder.selectAsBase(emptyCell)
      }
    }
  }

  def fillExposedNook(nookCell : ExpressionBuilder#GalleryCell, targetId : List[IdentToken], fillerId : List[IdentToken]) = {
    for {
      exprBuilder <- expressionBuilder
    } {
      if (nookCell.owner.isExposedNook) {
        exprBuilder.deselectAll

        val nook = nookCell.owner.getSimpleFramework.toCell

        val (targetIsThin, targetCell) =
          if (nookCell.owner.isOutNook) {
            ((true /: (nookCell.owner.sources.get map (_.isThin))) (_&&_), nookCell.owner.target.get)
          } else {
            (nookCell.owner.target.get.isThin, nookCell.owner.emptySources.head)
          }

        targetCell.item = Neutral(Some(FillerFace(targetId, nook, targetIsThin)))
        val tgtExprCell = targetCell.getSimpleFramework.toCell map (_.get)
        environment += tgtExprCell

        nookCell.owner.item = Neutral(Some(Filler(fillerId, nook)))
        val exprCell = nookCell.owner.getSimpleFramework.toCell map (_.get)
        environment += exprCell
      }
    }
  }

  def fillFromEnvironment(emptyCell : ExpressionBuilder#GalleryCell, expr : NCell[Expression]) = {
    for {
      exprBuilder <- expressionBuilder
    } {
      val complex = exprBuilder.complex

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
            exprBuilder.deselectAll

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

      // Overkill!!!
      exprBuilder.refreshAll
    }
  }

  def newDefnFromEnv(id : String) = {
    if (environment.length > 0) {
      val defnEnv = new ListBuffer[NCell[Expression]]
      environment.copyToBuffer(defnEnv)
      val defn = new Definition(id, defnEnv)

      // Make the necessary tree cells to represent this definition in the viewer
      val defnHyposItem = new TreeItem[DefinitionTreeItem] {
        value = HypothesesItem
        children ++= defn.freeVariables map (expr => new TreeItem[DefinitionTreeItem] { value = ExpressionItem(expr) }.delegate)
      }

      val defnConclusionItem = new TreeItem[DefinitionTreeItem] {
        value = ConclusionItem
        children ++= defn.derivedCells map (expr => new TreeItem[DefinitionTreeItem] { value = ExpressionItem(expr) }.delegate)
      }

      val defnTreeItem = new TreeItem[DefinitionTreeItem] {
        value = DefinitionItem(defn)
        children ++= List(defnHyposItem, defnConclusionItem)
      }

      definitionTreeRoot.children += defnTreeItem

      clearEnvironment

      println("After clear, defn has " ++ defn.environment.length.toString ++ "cells.")
    }
  }

  //============================================================================================
  // EDITOR HELPER ROUTINES
  //

  var sheetCount = 1

  def newSheet : Unit = newSheet(Object(None))

  def newSheet(seed : NCell[Option[Expression]]) = {
    val builder = new ExpressionBuilder(CardinalComplex(seed))
    val sheet = new Tab { text = "Sheet " ++ sheetCount.toString ; content = builder }
    sheetCount += 1
    sheetTabPane += sheet
    sheetTabPane.selectionModel().select(sheet)
    //reactTo(builder)
    builder.renderAll
  }

  def setPreview(expr : NCell[Expression]) = {
    val gallery = new FrameworkGallery(expr map (e => Some(e)))
    gallery.renderAll
    previewPane.content = gallery
    previewPane.requestLayout
  }

  def expressionBuilder : Option[ExpressionBuilder] = {
    val builder = sheetTabPane.getSelectionModel.selectedItem().content().asInstanceOf[ExpressionBuilder]
    if (builder != null) Some(builder) else None
  }

  def clearEnvironment = {
    sheetTabPane.tabs.clear
    environment.clear
    sheetCount = 1
    newSheet
  }

  newSheet
}

// class Editor extends PopupManager(new VBox) { thisEditor =>

//   implicit val pm = thisEditor

//   val mainVBox = new VBox(root.delegate.asInstanceOf[jfxsl.VBox])

//   // val newDefinitionItem = new MenuItem {
//   //   text = "New Definition"
//   //   onAction = onNewDefinition
//   // }

//   // val newSheetItem = new MenuItem {
//   //   text = "New Sheet"
//   //   onAction = onNewSheet
//   // }

//   val exitItem = new MenuItem {
//     text = "Exit"
//     onAction = javafx.application.Platform.exit
//   }

//   val fileMenu = new Menu {
//     text = "File"
//     items ++= List(exitItem)
//   }

//   // val completeDefinitionItem = new MenuItem {
//   //   text = "Complete With Cell"
//   //   onAction = onCompleteDefinition
//   // }

//   // val definitionMenu = new Menu {
//   //   text = "Definition"
//   //   items ++= List(completeDefinitionItem)
//   // }

//   val menuBar = new MenuBar {
//     menus ++= List(fileMenu)
//   }

//   val sheetTabPane = new TabPane {
//     side = Side.BOTTOM
//   }

//   sealed trait DefinitionTreeItem
//   case class DefinitionItem(defn : Definition) extends DefinitionTreeItem { override def toString = defn.toString }
//   case object HypothesesItem extends DefinitionTreeItem { override def toString = "Hypotheses" }
//   case object ConclusionItem extends DefinitionTreeItem { override def toString = "Conclusions" }
//   case class ExpressionItem(expr : NCell[Expression]) extends DefinitionTreeItem { override def toString = expr.value.id }

//   class DefinitionTreeCell extends jfxsc.TreeCell[DefinitionTreeItem] {

//     getStyleClass add "orch-list-cell"

//     var lastStyle : Option[String] = None

//     def setStyleType(styleType : String) = {
//       lastStyle foreach (st => getStyleClass remove st)
//       getStyleClass add styleType
//       lastStyle = Some(styleType)
//     }

//     def clearStyleType = {
//       lastStyle foreach (st => getStyleClass remove st)
//       lastStyle = None
//     }

//     override def updateItem(defnTreeItem : DefinitionTreeItem, empty : Boolean) = {
//       super.updateItem(defnTreeItem, empty)

//       if (! empty) {
//         defnTreeItem match {
//           case DefinitionItem(defn) => { clearStyleType ; setText(defn.toString) }
//           case ExpressionItem(expr) => {
//             expr.value match {
//               case Variable(_, isThin) => {
//                 if (isThin) {
//                   setStyleType("orch-list-cell-var-thin")
//                 } else {
//                   setStyleType("orch-list-cell-var")
//                 }
//               }
//               case Filler(_, _, _) => setStyleType("orch-list-cell-filler")
//               case FillerFace(_, _, isThin) => {
//                 if (isThin) {
//                   setStyleType("orch-list-cell-filler-face-thin")
//                 } else {
//                   setStyleType("orch-list-cell-filler-face")
//                 }
//               }
//             }

//             setText(expr.toString)
//           }
//           case item @ _ => { clearStyleType ; setText(item.toString) }
//         }
//       }
//     }
//   }

//   val definitionTreeRoot = new TreeItem[DefinitionTreeItem]
//   val definitionTreeView = 
//     new TreeView[DefinitionTreeItem] {
//       root = definitionTreeRoot
//       showRoot = false
//       cellFactory = (_ => new DefinitionTreeCell)
//     }

//   val sheetPane = new StackPane {
//     content = sheetTabPane
//     padding = Insets(10,10,10,10)
//     styleClass += "orch-pane"
//   }

//   val definitionTreePane = {
//     val defnTitledPane = 
//       new TitledPane {
//         text = "Definitions"
//         content = definitionTreeView
//         collapsible = false
//         expanded = true
//       }

//     AnchorPane.setTopAnchor(defnTitledPane, 10)
//     AnchorPane.setRightAnchor(defnTitledPane, 10)
//     AnchorPane.setBottomAnchor(defnTitledPane, 10)
//     AnchorPane.setLeftAnchor(defnTitledPane, 10)

//     new AnchorPane {
//       content = defnTitledPane
//       styleClass += "orch-pane"
//     }
//   }

//   val horizontalSplit = new SplitPane {
//     orientation = Orientation.HORIZONTAL
//     items.addAll(definitionTreePane, definitionBuilderPane)
//     dividerPositions = 0.2f
//   }

//   VBox.setVgrow(horizontalSplit, Priority.ALWAYS)
//   mainVBox.content.addAll(menuBar, horizontalSplit)

//   //============================================================================================
//   // DIALOG DEFINITIONS
//   //

//   // object NewDefinitionDialog extends Dialog {

//   //   heading.text = "New Definition"

//   //   val nameField = new TextField { promptText = "Definition name" }

//   //   borderPane.center = 
//   //     new VBox {
//   //       padding = Insets(10,10,10,10)
//   //       spacing = 10
//   //       content = List(nameField)
//   //     }

//   //   def onShow = ()

//   //   def onHide = {
//   //     newDefinition(nameField.text())
//   //     nameField.clear
//   //   }

//   // }

//   abstract class FillingDialog(freeVars : Seq[NCell[Expression]]) extends CancellableDialog {

//     val freeVarList = new ListView[NCell[Expression]] {
//       items = ObservableBuffer(freeVars)
//       cellFactory = (_ => {
//         val newCell = new EnvironmentCell
//         newCell
//       })
//     }

//     val composeField = new TextField { promptText = "Composite" ; onAction = () => { fillerField.requestFocus } }
//     val fillerField = new TextField { promptText = "Filler" ; onAction = () => { okBtn.fire } }

//     borderPane.center = 
//       new VBox {
//         padding = Insets(10,10,10,10)
//         spacing = 10
//         content = List(freeVarList, composeField, fillerField)
//       }

//     def onShow = {
//       composeField.clear
//       fillerField.clear
//       composeField.requestFocus
//     }

//     def parseResults(defnBuilder : JavaFXDefinitionBuilder) : Option[(List[IdentToken], List[IdentToken])] = {

//       import IdentParser.Success
//       import IdentParser.NoSuccess

//       IdentParser(composeField.text()) match {
//         case Success(composeIdent, _) => {
//           IdentParser(fillerField.text()) match {
//             case Success(fillerIdent, _) => {

//               def validRef(ident : IdentToken) : Boolean = 
//                 ident match {
//                   case LiteralToken(_) => true
//                   case ReferenceToken(id) => freeVars exists (expr => expr.value.id == id)
//                 }

//               val validRefs = (true /: ((composeIdent ++ fillerIdent) map (validRef(_)))) (_&&_)

//               if (validRefs) {
//                 // BUG: Does not check that the two are not given the *same* name ... and
//                 // BUG: We shouldn't allow the empty string.

//                 if (defnBuilder.envContains(IdentToken.getId(composeIdent)) ||
//                   defnBuilder.envContains(IdentToken.getId(fillerIdent))) { println("Duplicate identifier.") ; None }
//                 else Some(composeIdent, fillerIdent)
//               } else { println("Missing a variable.") ; None }
//             }
//             case _ : NoSuccess => { println("Filler parse failed.") ; None }
//           }
//         }
//         case _ : NoSuccess => { println("Compose parse failed.") ; None }
//       }
//     }
//   }

//   class FillNookDialog(nookCell : ExpressionBuilder#GalleryCell) 
//       extends FillingDialog(nookCell.owner.getSimpleFramework.freeVariables.values.toSeq) {

//     heading.text = "Fill Nook"

//     def onHide =
//       response match {
//         case DialogOK =>
//           for {
//             defnBuilder <- definitionBuilder
//             exprBuilder <- expressionBuilder
//             (composeIdent, fillerIdent) <- parseResults(defnBuilder)
//           } {
//             defnBuilder.fillExposedNook(nookCell, composeIdent, fillerIdent)
//           }
//         case DialogCancel => ()
//       }

//   }

//   class IdentityDialog(freeVars : Seq[NCell[Expression]], expr : Expression) extends FillingDialog(freeVars) {

//     heading.text = "Insert Identity"

//     composeField.text = "id-${" ++ expr.id ++ "}"
//     fillerField.text = "def-id-${" ++ expr.id ++ "}"

//     override def onShow = { composeField.requestFocus }

//     def onHide =
//       response match {
//         case DialogOK => 
//           for {
//             defnBuilder <- definitionBuilder
//             exprBuilder <- expressionBuilder
//             (composeIdent, fillerIdent) <- parseResults(defnBuilder)
//           } {
//             exprBuilder.extrudeDrop
//             defnBuilder.fillExposedNook(exprBuilder.lastFiller, composeIdent, fillerIdent)
//           }
//         case DialogCancel => ()
//       }

//   }

//   class ComposeDialog(freeVars : Seq[NCell[Expression]]) extends FillingDialog(freeVars) {

//     heading.text = "Insert Composite"

//     def onHide = 
//       response match {
//         case DialogOK => ()
//           for {
//             defnBuilder <- definitionBuilder
//             exprBuilder <- expressionBuilder
//             (composeIdent, fillerIdent) <- parseResults(defnBuilder)
//           } {
//             exprBuilder.extrudeSelection
//             defnBuilder.fillExposedNook(exprBuilder.lastFiller, composeIdent, fillerIdent)
//           }
//         case DialogCancel => ()
//       }
//   }

//   object VariableDialog extends CancellableDialog {

//     heading.text = "Assume Variable"

//     val idField = new TextField { promptText = "Identifier" ; onAction = () => { okBtn.fire } }
//     val thinCheckBox = new CheckBox("Thin") { allowIndeterminate = false }

//     borderPane.center = 
//       new VBox {
//         padding = Insets(10,10,10,10)
//         spacing = 10
//         content = List(idField, thinCheckBox)
//       }

//     def onShow = {
//       idField.clear
//       idField.requestFocus
//     }

//     def onHide =
//       response match {
//         case DialogOK => {
//           for {
//             defnBuilder <- definitionBuilder
//             exprBuilder <- defnBuilder.activeBuilder
//           } {
//             if (defnBuilder.envContains(idField.text())) {
//               println("Error: Duplicate Identifier")
//             } else {
//               defnBuilder.assume(idField.text(), thinCheckBox.selected())
//             }
//           }
//         }
//         case DialogCancel => ()
//       }

//   }

//   //============================================================================================
//   // EVENTS
//   //

//   addEventFilter(KeyEvent.KEY_PRESSED,
//     new EventHandler[KeyEvent] {
//       def handle(ev : KeyEvent) {
//         ev.getCode match {
//           case KeyCode.LEFT => for { exprBuilder <- expressionBuilder } exprBuilder.prev 
//           case KeyCode.RIGHT => for { exprBuilder <- expressionBuilder } exprBuilder.next
//           case KeyCode.E => if (ev.isControlDown) onExtrude
//           case KeyCode.D => if (ev.isControlDown) onDrop
//           case KeyCode.A => if (ev.isControlDown) onAssume(ev.isShiftDown)
//           case KeyCode.C => if (ev.isControlDown) onCompose
//           case KeyCode.I => if (ev.isControlDown) onInsertIdentity
//           case KeyCode.F => if (ev.isControlDown) onFill  
//           case KeyCode.U => if (ev.isControlDown) onUseEnvironment
//           // case KeyCode.V => if (ev.isControlDown) onView
//           // case KeyCode.O => if (ev.isControlDown) onOpen
//           // case KeyCode.S => if (ev.isControlDown) onSave
//           case KeyCode.N => if (ev.isControlDown) onNewSheet
//           // case KeyCode.L => if (ev.isControlDown) onLoadExpr
//           // case KeyCode.G => if (ev.isControlDown) onGlobCardinal
//           // case KeyCode.X => if (ev.isControlDown) onExtra
//           // case KeyCode.P => if (ev.isControlDown) onPrintScreen
//           // case KeyCode.W => if (ev.isControlDown) onWebView
//           // case KeyCode.M => if (ev.isControlDown) displayMessage("Message", "This is a message!")
//           // case KeyCode.Z => if (ev.isControlDown) { debug = ! debug ; println("Debug is now: " ++ (if (debug) "on" else "off")) }
//           case _ => ()
//         }
//       }
//     })

//   def onExtrude = {
//     for { exprBuilder <- expressionBuilder } {
//       exprBuilder.extrudeSelection
//     }
//   }

//   def onDrop = {
//     for { exprBuilder <- expressionBuilder } {
//       exprBuilder.extrudeDrop
//     }
//   }

//   def onAssume(thin : Boolean) = {
//     for { 
//       exprBuilder <- expressionBuilder
//       cell <- exprBuilder.selectionBase
//     } {
//       if (cell.owner.isShell) {
//         VariableDialog.thinCheckBox.selected = thin
//         VariableDialog.run
//       } else {
//         println("Error: selection is not a shell!")
//       }
//     }
//   }

//   def onCompose = {
//     for { exprBuilder <- expressionBuilder } {
//       if (exprBuilder.selectionIsComposable) {
//         val freeVars = HashMap.empty[String, NCell[Expression]]

//         exprBuilder.selectedCells foreach (cell => {
//           cell.owner.getSimpleFramework.collectDependencies(freeVars)
//         })

//         freeVars filter (pr => {
//           val (id, expr) = pr
//           expr.value match {
//             case Variable(_, _) => true
//             case _ => false
//           }
//         })
        
//         new ComposeDialog(freeVars.values.toSeq).run
//       }
//     }
//   }

//   def onInsertIdentity = {
//     for { exprBuilder <- expressionBuilder } {
//       if (exprBuilder.selectionIsComposable) {
//         for { cell <- exprBuilder.selectionBase } {
//           cell.item match {
//             case Neutral(Some(expr)) => {
//               val freeVars = cell.owner.getSimpleFramework.freeVariables.values.toSeq
//               new IdentityDialog(freeVars, expr).run
//             }
//             case _ => ()
//           }
//         }
//       }
//     }
//   }

//   def onFill = {
//     for {
//       exprBuilder <- expressionBuilder
//       selectedCell <- exprBuilder.selectionBase
//     } {
//       if (selectedCell.owner.isExposedNook) {
//         new FillNookDialog(selectedCell).run
//       }
//     }
//   }

//   def onUseEnvironment = {
//     for { 
//       defnBuilder <- definitionBuilder
//       exprBuilder <- expressionBuilder
//       selectedCell <- exprBuilder.selectionBase
//     } {
//       if (selectedCell.owner.isEmpty) {
//         val selectedExpr = defnBuilder.envListView.getSelectionModel.getSelectedItem

//         if (selectedExpr != null) {
//           defnBuilder.fillFromEnvironment(selectedCell, selectedExpr)
//         }
//       }
//     }
//   }
  
//   def onNewDefinition = {
//     NewDefinitionDialog.run
//   }

//   def onNewSheet = {
//     for { defnBldr <- definitionBuilder } { 
//       defnBldr.newSheet 
//     }
//   }

//   def onCompleteDefinition = {
//     for {
//       defnBuilder <- definitionBuilder
//     } {
//       val selectedExpr = defnBuilder.envListView.getSelectionModel.getSelectedItem

//       if (selectedExpr != null) {
//         selectedExpr.value match {
//           case fillerExpr @ Filler(_, _, _) => {
//             val defn = new Definition(defnBuilder.text(), fillerExpr)

//             // Could check and print a warning if there are unused cells in the environment ...

//             // Make the necessary tree cells to represent this definition in the viewer
//             val defnHyposItem = new TreeItem[DefinitionTreeItem] {
//               value = HypothesesItem
//               children ++= defn.freeVariables map (expr => new TreeItem[DefinitionTreeItem] { value = ExpressionItem(expr) }.delegate)
//             }

//             val defnConclusionItem = new TreeItem[DefinitionTreeItem] {
//               value = ConclusionItem
//               children += new TreeItem[DefinitionTreeItem] { value = new ExpressionItem(defn.faceCell) }.delegate
//               children += new TreeItem[DefinitionTreeItem] { value = new ExpressionItem(defn.defCell) }.delegate
//             }

//             val defnTreeItem = new TreeItem[DefinitionTreeItem] {
//               value = DefinitionItem(defn)
//               children ++= List(defnHyposItem, defnConclusionItem)
//             }

//             definitionTreeRoot.children += defnTreeItem
//             definitionTabPane.tabs -= defnBuilder
//           }
//           case _ => println("Definition must be a filling cell.")
//         }
//       }
//     }
//   }

//   //============================================================================================
//   // SEMANTICS
//   //

//   def newDefinition(name : String) = {
//     val builder = new JavaFXDefinitionBuilder
//     builder.text = name
//     definitionTabPane.tabs += builder
//     definitionTabPane.getSelectionModel.select(builder)
//   }

//   def definitionBuilder : Option[JavaFXDefinitionBuilder] = {
//     val activeDef = 
//       definitionTabPane.getSelectionModel.
//         selectedItem().asInstanceOf[JavaFXDefinitionBuilder]

//     if (activeDef != null) Some(activeDef) else None
//   }

//   def expressionBuilder : Option[ExpressionBuilder] = {
//     for {
//       defnBuilder <- definitionBuilder
//       exprBuilder <- defnBuilder.activeBuilder
//     } yield exprBuilder
//   }


// }

object Editor extends JFXApp {

  val orchardScene = new Scene(new Editor, 1600, 900)

  orchardScene.getStylesheets.add("orchard/ui/javafx/OrchardUI.css")

  stage = new JFXApp.PrimaryStage {
      title = "Orchard"
      scene = orchardScene
    }

}
