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

class Editor extends PopupManager(new VBox) with OrchardMenus { thisEditor =>

  override implicit def pm = thisEditor

  val mainVBox = new VBox(root.delegate.asInstanceOf[jfxsl.VBox])

  val fileChooser = new FileChooser

  val sheetTabPane = new TabPane {
    side = Side.TOP
  }

  val sheetPane = new StackPane {
    content = sheetTabPane
    padding = Insets(10,10,10,10)
    styleClass += "orch-pane"
  }

  // Probably should change to the selection onChange method used
  // below in the navigation tree
  val environmentListView = 
    new ListView[NCell[Expression]] {
      items = ObservableBuffer.empty[NCell[Expression]]
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

  val goalsListView = 
    new ListView[NCell[Expression]] {
      items = ObservableBuffer.empty[NCell[Expression]]
      cellFactory = (_ =>
        new EnvironmentCell {
          setOnMouseClicked(new EventHandler[MouseEvent] {
            def handle(ev : MouseEvent) {
              if (! isEmpty) {
                setPreview(getItem)

                // if (ev.getClickCount > 1) {
                //   newSheet(getItem map (e => Some(e)))
                // }
              }
            }
          })
        })
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
  case object VariablesItem extends DefinitionTreeItem { override def toString = "Free Variables" }
  case object ResultItem extends DefinitionTreeItem { override def toString = "Resulting Cell" }
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
  // DIALOG DEFINITIONS
  //

  abstract class FillingDialog extends CancellableDialog {

    val freeVars = selectionFreeVariables.get

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

    def parseResults(wksp : Workspace) : Option[(Identifier, Identifier)] = {

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

              val validRefs = (true /: ((composeIdent.tokens ++ fillerIdent.tokens) map (validRef(_)))) (_&&_)

              if (validRefs) {
                // BUG: Does not check that the two are not given the *same* name ... and
                // BUG: We shouldn't allow the empty string.

                if (wksp.environmentContains(composeIdent.toString) ||
                  wksp.environmentContains(fillerIdent.toString)) { println("Duplicate identifier.") ; None }
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

  class FillNookDialog extends FillingDialog {

    heading.text = "Fill Nook"

    def onHide =
      response match {
        case DialogOK =>
          for {
            wksp <- activeWorkspace
            exprBuilder <- expressionBuilder
            nookCell <- exprBuilder.selectionBase
            (composeIdent, fillerIdent) <- parseResults(wksp)
          } {
            fillExposedNook(nookCell, composeIdent, fillerIdent)
          }
        case DialogCancel => ()
      }

  }

  class IdentityDialog(expr : Expression) extends FillingDialog {

    heading.text = "Insert Identity"

    composeField.text = "id-${" ++ expr.id ++ "}"
    fillerField.text = "def-id-${" ++ expr.id ++ "}"

    override def onShow = { composeField.requestFocus }

    def onHide =
      response match {
        case DialogOK => 
          for {
            wksp <- activeWorkspace
            exprBuilder <- expressionBuilder
            (composeIdent, fillerIdent) <- parseResults(wksp)
          } {
            exprBuilder.extrudeDrop
            fillExposedNook(exprBuilder.lastFiller, composeIdent, fillerIdent)
          }
        case DialogCancel => ()
      }

  }

  class ComposeDialog extends FillingDialog {

    heading.text = "Insert Composite"

    def onHide = 
      response match {
        case DialogOK => ()
          for {
            wksp <- activeWorkspace
            exprBuilder <- expressionBuilder
            (composeIdent, fillerIdent) <- parseResults(wksp)
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
          for {
            wksp <- activeWorkspace
          } {
            if (wksp.environmentContains(idField.text())) {
              println("Error: Duplicate Identifier")
            } else {
              assume(idField.text(), thinCheckBox.selected())
            }
          }
        }
        case DialogCancel => ()
      }

  }

  object NewDefinitionDialog extends CancellableDialog {

    heading.text = "New Definition"

    val nameLabel = new Label("Name: ")
    val nameField = new TextField { promptText = "Definition name" }
    val nameHBox = new HBox { 
      content = List(nameLabel, nameField) 
      alignment = Pos.CENTER
      spacing = 5
      padding = Insets(0, 10, 10, 10)
    }

    GridPane.setColumnSpan(nameHBox, 2)

    val stableButton = new RadioButton("Stable") {
      onAction = { () => 
        stabilityField.text = "infty" 
        stabilitySlider.disable = true
        stabilityField.disable = true
      }
    }

    val unstableButton = new RadioButton("Unstable") {
      onAction = { () => 
        stabilityField.disable = false
        stabilitySlider.disable = false
        stabilityField.text = stabilitySlider.value().toInt.toString 
      }
    }

    val stabilityToggle = new ToggleGroup {
      toggles = List(stableButton, unstableButton)
    }

    val stabilitySlider = new Slider { 
      min = 0
      max = 10
      majorTickUnit = 1
      minorTickCount = 0
      snapToTicks = true
      showTickMarks = false
    }

    stabilitySlider.value onChange {
      stabilityField.text = stabilitySlider.value().toInt.toString
    }

    val stabilityField = new TextField { editable = false }

    val noInvertibilityButton = new RadioButton("No Invertibility") {
      onAction = { () =>  ()
        invertibilityField.text = "infty" 
        invertibilitySlider.disable = true
        invertibilityField.disable = true
      }
    }

    val finiteInvertibilityButton = new RadioButton("Finite Invertibility") {
      onAction = { () => ()
        invertibilityField.disable = false
        invertibilitySlider.disable = false
        invertibilityField.text = invertibilitySlider.value().toInt.toString 
      }
    }

    val invertibilityToggle = new ToggleGroup {
      toggles = List(noInvertibilityButton, finiteInvertibilityButton)
    }

    val invertibilitySlider = new Slider {
      min = 0
      max = 10
      majorTickUnit = 1
      minorTickCount = 0
      snapToTicks = true
      showTickMarks = false
    }

    invertibilitySlider.value onChange {
      invertibilityField.text = invertibilitySlider.value().toInt.toString
    }

    val invertibilityField = new TextField { editable = false }

    val noUnicityButton = new RadioButton("No Unicity") {
      onAction = { () =>  ()
        unicityField.text = "infty" 
        unicitySlider.disable = true
        unicityField.disable = true
      }
    }

    val finiteUnicityButton = new RadioButton("Finite Unicity") {
      onAction = { () => ()
        unicityField.disable = false
        unicitySlider.disable = false
        unicityField.text = unicitySlider.value().toInt.toString 
      }
    }

    val unicityToggle = new ToggleGroup {
      toggles = List(noUnicityButton, finiteUnicityButton)
    }

    val unicitySlider = new Slider {
      min = 0
      max = 10
      majorTickUnit = 1
      minorTickCount = 0
      snapToTicks = true
      showTickMarks = false
    }

    unicitySlider.value onChange {
      unicityField.text = unicitySlider.value().toInt.toString
    }

    val unicityField = new TextField { editable = false }

    val gridPane = new GridPane {
      padding = Insets(10, 10, 10, 10)
      styleClass += "orch-pane"
      hgap = 5
      vgap = 5
    }

    gridPane.add(nameHBox, 1, 1)
    gridPane.add(stableButton, 1, 2)
    gridPane.add(unstableButton, 2, 2)
    gridPane.add(stabilitySlider, 1, 3)
    gridPane.add(stabilityField, 2, 3)
    gridPane.add(noInvertibilityButton, 1, 4)
    gridPane.add(finiteInvertibilityButton, 2, 4)
    gridPane.add(invertibilitySlider, 1, 5)
    gridPane.add(invertibilityField, 2, 5)
    gridPane.add(noUnicityButton, 1, 6)
    gridPane.add(finiteUnicityButton, 2, 6)
    gridPane.add(unicitySlider, 1, 7)
    gridPane.add(unicityField, 2, 7)

    borderPane.center = gridPane

    def onShow = {
      unstableButton.fire
      stabilitySlider.value = 0
      noInvertibilityButton.fire
      noUnicityButton.fire
      nameField.requestFocus
    }

    def onHide = 
      response match {
        case DialogOK => {
          val id : String = nameField.text()

          val stabilityLevel : Option[Int] = 
            if (unstableButton.selected()) {
              Some(stabilityField.text().toInt)
            } else None

          val invertibilityLevel : Option[Int] = 
            if (finiteInvertibilityButton.selected()) {
              Some(invertibilityField.text().toInt)
            } else None

          val unicityLevel : Option[Int] = 
            if (finiteUnicityButton.selected()) {
              Some(unicityField.text().toInt)
            } else None

          val wksp = new JavaFXDefinitionWorkspace(id, stabilityLevel, invertibilityLevel, unicityLevel)
          wksp.sheets += new ExpressionBuilder(CardinalComplex(Object(None)))

          val navTreeItem = new TreeItem[NavigationTreeItem] {
            value = DefnWorkspaceItem(wksp)
          }

          // I guess what you could do here is allow the use to view
          // the environment ... but not sure if it's necessary

          navigationTreeRoot.children += navTreeItem
          navigationTreeView.getSelectionModel.select(navTreeItem)
        }
        case DialogCancel => ()
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
          case KeyCode.O => if (ev.isControlDown) onOpen
          case KeyCode.S => if (ev.isControlDown) onSave
          // case KeyCode.V => if (ev.isControlDown) onView
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

  def onCompose = (new ComposeDialog).run

  def onInsertIdentity = {
    for { 
      wksp <- activeWorkspace
      exprBuilder <- expressionBuilder 
    } {
      if (exprBuilder.selectionIsComposable && exprBuilder.selectionIsUnique) {
        for { cell <- exprBuilder.selectionBase } {
          cell.item match {
            case Neutral(Some(expr)) => {
              (new IdentityDialog(expr)).run
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
        (new FillNookDialog).run
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

  def onNewDefinition = {
    NewDefinitionDialog.run
  }

  def onCompleteDefinition = {
    for {
      wksp <- activeWorkspace
    } {
      if (wksp.isInstanceOf[DefinitionWorkspace]) {
        val defnWksp = wksp.asInstanceOf[DefinitionWorkspace]

        val selectedExpression : NCell[Expression] = 
          environmentListView.getSelectionModel.selectedItem()

        if (selectedExpression != null) {
          val defnExpression = 
            selectedExpression.value match {
              case Variable(_, _) => selectedExpression
              case Filler(_) => selectedExpression
              case FillerFace(_, filler, _) => wksp.getFromEnvironment(filler).get
            }

          val deps = wksp.dependencies(SimpleFramework(defnExpression)).values.toSeq

          // Great, so this I think should trim the definition environment down to
          // exactly the cells which are used ...
          val defnEnvironment = 
            wksp.environment filter (expr => deps exists (e => e.value.id == expr.value.id))

          defnEnvironment += defnExpression

          val defn =
            new Definition(defnWksp.name,
              defnWksp.stabilityLevel,
              defnWksp.invertibilityLevel,
              defnWksp.unicityLevel,
              defnExpression,
              defnEnvironment)

          addLocalDefinition(defn)
          closeActiveWorkspace
          accordion.expandedPane = definitionsPane
        }
      }
    }
  }

  def onSpawnInShell = 
    for { 
      wksp <- activeWorkspace
      exprBuilder <- expressionBuilder
      cell <- exprBuilder.selectionBase
    } {
      if (cell.owner.hasCompleteShell) {
        // Now we need to find the definition we are spawning.

        val selectedWkspItem = navigationTreeView.getSelectionModel.selectedItem()
        val selectedDefnItem = definitionTreeView.getSelectionModel.selectedItem()

        if (selectedDefnItem != null && selectedWkspItem != null) {
          val selectedDefn = findParentDefinition(selectedDefnItem)
          val selectedShell = cell.owner.getSimpleFramework

          val substWksp = 
            new JavaFXSubstitutionWorkspace(selectedDefn.name, 
                                            wksp,
                                            selectedDefn,
                                            selectedShell)

          // Import the current environment and create a blank sheet
          substWksp.environment ++= wksp.environment
          substWksp.sheets += new ExpressionBuilder(CardinalComplex(Object(None)))

          val navTreeItem = new TreeItem[NavigationTreeItem] {
            value = SubstWorkspaceItem(substWksp)
          }

          // We should generate more tree items for the goals now

          selectedWkspItem.children += navTreeItem
          navigationTreeView.getSelectionModel.select(navTreeItem)

        } else {
          println("No definition selected.")
        }
      } else {
        println("Cannot spawn substitution in incomplete shell.")
      }
    }

  def onSave = {
    fileChooser.setTitle("Save")

    val file = fileChooser.showSaveDialog(getScene.getWindow)

    if (file != null) {
      saveDefinitions(file)
    }
  }

  def onOpen = {
    fileChooser.setTitle("Open")

    val file = fileChooser.showOpenDialog(getScene.getWindow)

    if (file != null) {
      loadDefinitions(file)
    }
  }

  def onExit = javafx.application.Platform.exit

  //============================================================================================
  // SEMANTIC ROUTINES
  //

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

        val exprCell : NCell[Expression] = emptyCell.owner.getSimpleFramework.toExpressionCell
        addToActiveEnvironment(exprCell)

        exprBuilder.selectAsBase(emptyCell)
      }
    }
  }

  def fillExposedNook(nookCell : ExpressionBuilder#GalleryCell, targetIdent : Identifier, fillerIdent : Identifier) = {
    for {
      exprBuilder <- expressionBuilder
    } {
      if (nookCell.owner.isExposedNook) {
        exprBuilder.deselectAll

        val (targetIsThin, targetCell) =
          if (nookCell.owner.isOutNook) {
            ((true /: (nookCell.owner.sources.get map (_.isThin))) (_&&_), nookCell.owner.target.get)
          } else {
            (nookCell.owner.target.get.isThin, nookCell.owner.emptySources.head)
          }

        val filler = Filler(fillerIdent)
        nookCell.owner.item = Neutral(Some(filler))
        targetCell.item = Neutral(Some(FillerFace(targetIdent, filler.id, targetIsThin)))

        addToActiveEnvironment(targetCell.getSimpleFramework.toExpressionCell)
        addToActiveEnvironment(nookCell.owner.getSimpleFramework.toExpressionCell)
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

  //============================================================================================
  // EDITOR HELPER ROUTINES
  //

  var activeWorkspace : Option[JavaFXWorkspace] = None

  def selectWorkspace(wksp : JavaFXWorkspace) = {
    sheetTabPane.tabs.clear
    sheetCount = 1

    wksp.sheets foreach (sheet => {
      val builder = sheet

      val tab = new Tab {
        text = "Sheet " ++ sheetCount.toString
        content = builder
      }

      sheetTabPane += tab
      sheetCount += 1
      builder.refreshAll
    })

    environmentListView.items = ObservableBuffer(wksp.environment)
    goalsListView.items = 
      if (wksp.isInstanceOf[SubstitutionWorkspace]) {
        ObservableBuffer(wksp.asInstanceOf[SubstitutionWorkspace].goals)
      } else
        ObservableBuffer.empty[NCell[Expression]]

    activeWorkspace = Some(wksp)
  }

  def closeActiveWorkspace =
    for { wksp <- activeWorkspace } { closeWorkspace(wksp) }

  // This only works for definition workspaces.  What we should really do
  // is have this work more generally
  def closeWorkspace(wksp : Workspace) = {
    var wkspItem : Option[TreeItem[NavigationTreeItem]] = None

    navigationTreeRoot.children foreach (child => {
      if (child.value().asInstanceOf[DefnWorkspaceItem].wksp.name == wksp.name)
        wkspItem = Some(child)
    })

    wkspItem foreach (item => navigationTreeRoot.children remove item)

    environmentListView.items = ObservableBuffer.empty[NCell[Expression]]
    sheetTabPane.tabs.clear
    activeWorkspace = None
  }

  def selectionFreeVariables : Option[Seq[NCell[Expression]]] =
    for {
      wksp <- activeWorkspace
      exprBuilder <- expressionBuilder
    } yield {
      val freeVars = HashMap.empty[String, NCell[Expression]]

      exprBuilder.selectedCells foreach (cell => {
        wksp.collectFreeVars(cell.owner.getSimpleFramework, freeVars)
      })

      freeVars.values.toSeq
    }

  def selectionDependencies : Option[Seq[NCell[Expression]]] = None

  def addToActiveEnvironment(expr : NCell[Expression]) = 
    for {
      wksp <- activeWorkspace
    } {
      wksp.addToEnvironment(expr)
      environmentListView.items() += expr
    }

  var sheetCount = 1

  def newSheet : Unit = newSheet(Object(None))

  def newSheet(seed : NCell[Option[Expression]]) = 
    for {
      wksp <- activeWorkspace
    } {
      val builder = new ExpressionBuilder(CardinalComplex(seed))
      val sheet = new Tab { text = "Sheet " ++ sheetCount.toString ; content = builder }
      sheetCount += 1
      sheetTabPane += sheet
      sheetTabPane.selectionModel().select(sheet)
      // BUG! - When I close a sheet, it should get deleted from the workspace
      // More generally, it seems like there should be a tighter sheet integration
      // with the workspace somehow.
      wksp.sheets += builder
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

    val defnVariablesItem = new TreeItem[DefinitionTreeItem] {
      value = VariablesItem
      children ++= defn.environmentVariables map (expr => {
        new TreeItem[DefinitionTreeItem] {
          value = ExpressionItem(expr)
        }.delegate
      })
    }

    val defnResultItem = new TreeItem[DefinitionTreeItem] {
      value = ResultItem
      children += new TreeItem[DefinitionTreeItem] {
        value = ExpressionItem(defn.result)
      }.delegate
    }

    val defnTreeItem = new TreeItem[DefinitionTreeItem] {
      value = DefinitionItem(defn)
      children ++= List(defnVariablesItem, defnResultItem)
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

  val orchardScene = new Scene(new Editor, 1600, 900)

  orchardScene.getStylesheets.add("orchard/ui/javafx/OrchardUI.css")

  stage = new JFXApp.PrimaryStage {
      title = "Orchard"
      scene = orchardScene
    }

}
