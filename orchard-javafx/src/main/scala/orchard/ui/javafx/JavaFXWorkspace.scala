/**
  * JavaFXWorkspace.scala - Base trait for workspaces in JavaFX
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scala.collection.JavaConversions._
import scala.collection.mutable.Buffer

import scalafx.Includes._

import scalafx.geometry._

import scalafx.scene.layout._
import scalafx.scene.control._

import scalafx.collections.ObservableBuffer
import scalafx.collections.ObservableBuffer._

import javafx.{scene => jfxs}
import javafx.scene.{control => jfxsc}
import javafx.scene.text.Text

import orchard.core._

// import Environment._

sealed trait NavigationTreeItem
case class DefnWorkspaceItem(val wksp : JavaFXDefinitionWorkspace) extends NavigationTreeItem
case class SubstWorkspaceItem(val wksp : JavaFXSubstitutionWorkspace) extends NavigationTreeItem

trait JavaFXWorkspace extends Workspace 
    with WorksheetEnvironment 
    with ShapeEnvironment { thisWksp =>

  type EditorType = JavaFXEditor

  var activeExpression : Option[Expression[IndexType]] = None

  def getItemSeq(rootItem : TreeItem[Expression[IndexType]], treeItem : TreeItem[Expression[IndexType]]) : Seq[Int] = {
    var currentItem = treeItem
    val addr = Buffer.empty[Int]

    while (currentItem != rootItem) {
      val parentItem = currentItem.parent()
      parentItem.children.indexOf(currentItem) +=: addr
      currentItem = parentItem
    }

    addr
  }

  val treeItem = new TreeItem[JavaFXWorkspace] {
    value = thisWksp
  }

  def contextView : TreeView[Expression[DefinitionWorkspace#IndexType]]

  var sheetCount : Int = 1

  val sheetTabPane = new TabPane {
    side = Side.TOP
  }

  var activeGallery : Option[WorksheetGallery] = None 

  def activeSheet : Option[Worksheet] =
    for {
      gallery <- activeGallery
    } yield gallery.complex

  def newSheet = newSheet(Object(implicitly[HasEmpty[IndexType]].empty))

  def newSheet(seed : NCell[IndexType]) : Unit =
    newCardinalSheet(CardinalComplex(seed))

  def newGallery(seed : NCell[Polarity[IndexType]]) : WorksheetGallery

  def newCardinalSheet(seed : NCell[Polarity[IndexType]]) : Unit = {
    val gallery = newGallery(seed)

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

  def apply(defn : Definition) = applyInShell(Object(Seq.empty), defn)

  def applyInShell(shell : NCell[Seq[Int]], defn : Definition) : Option[JavaFXSubstitutionWorkspace] = {
    val substWksp =
      new JavaFXSubstitutionWorkspace(
        editor.asInstanceOf[JavaFXEditor],
        defn.name,
        this,
        defn,
        shell
      )

    // Copy the sheets
    sheets foreach (sheet => {
      val newSubstSheet = 
        sheet.toCell map (p => p map (substWksp.convertIndex(_)))

      substWksp.newCardinalSheet(newSubstSheet)
    })


    // Add the new workspace as a child
    treeItem.children += substWksp.treeItem

    Some(substWksp)
  }
}

class JavaFXDefinitionWorkspace(
  val editor : JavaFXEditor,
  val name : String,
  val stabilityLevel : Option[Int],
  val invertibilityLevel : Option[Int],
  val unicityLevel : Option[Int]
) extends DefinitionWorkspace with JavaFXWorkspace { thisWksp =>

  var activeExpressionIndex : Seq[Int] = Seq.empty

  val contextBuffer = ObservableBuffer.empty[Expression[IndexType]]

  val contextRoot = new TreeItem[Expression[IndexType]]
  val contextView =
    new TreeView[Expression[IndexType]] {
      root = contextRoot
      showRoot = false
      cellFactory = (_ => new ContextTreeCell)
    }

  contextView.getSelectionModel.selectedItem onChange {
    val exprItem = contextView.getSelectionModel.selectedItem()

    if (exprItem != null) {
      val expr = exprItem.value()
      activeExpression = Some(expr)
      activeExpressionIndex = getItemSeq(contextRoot, exprItem)

      // Now make a gallery and show it in the preview pane
      val gallery = new ShapeGallery(ShapeFramework(activeExpressionIndex))
      editor.setPreviewGallery(gallery)
    } else {
      activeExpression = None
      activeExpressionIndex = Seq.empty
    }

    for {
      wksp <- editor.activeWorkspace
    } {
      if (wksp.isInstanceOf[JavaFXSubstitutionWorkspace]) {
        wksp.asInstanceOf[JavaFXSubstitutionWorkspace].
          substContextView.getSelectionModel.clearSelection
      }
    }
  }

  // Use the actual changes, smart guy ...
  contextBuffer onChange { (_, chgs) =>
    chgs foreach {
      case Add(pos, added) => {
        val items =
          added.asInstanceOf[Traversable[Expression[Seq[Int]]]] map 
            (expr => buildExpressionTreeItem(expr).delegate)

        contextRoot.children.insertAll(pos, items)
      }
      case Remove(pos, removed) => ???
      case Reorder(_, _, _) => ???
    }
  }

  class ContextTreeCell extends jfxsc.TreeCell[Expression[IndexType]] {

    getStyleClass add "orch-list-cell"
    val cellStyleIndex = getStyleClass.length
    getStyleClass add "orch-list-null"

    def setCellStyleType(str : String) = {
      getStyleClass(cellStyleIndex) = str
    }

    override def updateItem(expr : Expression[IndexType], empty : Boolean) = {
      super.updateItem(expr, empty)

      if (! empty) {
        setCellStyleType("orch-list-cell-" ++ expr.styleString)
        setText(context.expandIdentifier(expr.ident))
      } else {
        setCellStyleType("orch-list-null")
        setText("")
      }
    } 
  }

  def buildExpressionTreeItem(expr : Expression[IndexType]) : TreeItem[Expression[IndexType]] = 
    expr match {
      case app @ Application(defn, _, _) => {
        val appItem = new TreeItem[Expression[IndexType]] { value = app }

        defn.contextBuffer foreach (e => {
          appItem.children += buildExpressionTreeItem(e).delegate
        })

        appItem
      }
      case other @ _ => new TreeItem[Expression[IndexType]] { value = other }
    }

  def completeDefinition : Option[Definition] =
    if (activeExpressionIndex.length == 1) {
      val defn = new Definition(name, stabilityLevel, invertibilityLevel, unicityLevel, contextBuffer, activeExpressionIndex.head)
      Some(defn)
    } else {
      println("Definition cannot be completed with the cell.")
      None
    }


  def newGallery(seed : NCell[Polarity[IndexType]]) : WorksheetGallery = 
    new DefinitionWorksheetGallery(seed)

  class DefinitionWorksheetPanel(val complex : DefinitionWorksheet, val baseIndex : Int) extends WorksheetPanel { thisPanel =>

    type CellType = DefinitionWorksheetPanelCell
    type EdgeType = DefinitionWorksheetPanelEdge

    type ComplexType = DefinitionWorksheet

    def newCell(owner : complex.DefinitionWorksheetCell) : DefinitionWorksheetPanelCell = {
      val cell = new DefinitionWorksheetPanelCell(owner)
      owner.registerPanelCell(thisPanel)(cell)
      reactTo(cell)
      cell
    }
    
    def newEdge(owner : complex.DefinitionWorksheetCell) : DefinitionWorksheetPanelEdge = {
      val edge = new DefinitionWorksheetPanelEdge(owner)
      owner.registerPanelEdge(thisPanel)(edge)
      reactTo(edge)
      edge
    }
    
    class DefinitionWorksheetPanelCell(val owner : complex.DefinitionWorksheetCell) extends WorksheetPanelCell
    class DefinitionWorksheetPanelEdge(val owner : complex.DefinitionWorksheetCell) extends WorksheetPanelEdge

    //============================================================================================
    // INITIALIZATION
    //

    var baseCell : DefinitionWorksheetPanelCell = newCell(complex.baseCells(baseIndex))

    refreshPanelData
    initializeChildren

  }

  class DefinitionWorksheetGallery(val complex : DefinitionWorksheet) extends WorksheetGallery {

    type PanelType = DefinitionWorksheetPanel

    def this(seed : NCell[Polarity[IndexType]]) = this(new DefinitionWorksheet(seed))

    def newPanel(i : Int) : DefinitionWorksheetPanel = {
      val panel = new DefinitionWorksheetPanel(complex, i)
      reactTo(panel)
      panel
    }

    initialize

  }

}

class JavaFXSubstitutionWorkspace(
  val editor : JavaFXEditor,
  val name : String,
  val parentWorkspace : JavaFXWorkspace,
  val defn : Definition,
  val shell : NCell[Seq[Int]]
) extends SubstitutionWorkspace with JavaFXWorkspace { thisWksp =>

  var activeExpressionIndex : SubstIndex = Internal(Seq.empty)
  
  def parentDefinitionWksp : JavaFXDefinitionWorkspace = 
    if (parentWorkspace.isInstanceOf[JavaFXDefinitionWorkspace]) {
      parentWorkspace.asInstanceOf[JavaFXDefinitionWorkspace]
    } else {
      parentWorkspace.asInstanceOf[JavaFXSubstitutionWorkspace].parentDefinitionWksp
    }

  def parentDefinition = parentDefinitionWksp

  def contextView = parentDefinitionWksp.contextView

  def stabilityLevel : Option[Int] = parentWorkspace.stabilityLevel
  def invertibilityLevel : Option[Int] = parentWorkspace.invertibilityLevel
  def unicityLevel : Option[Int] = parentWorkspace.unicityLevel

  val substContextRoot = buildSubstitutionContext(promoteExternalExpr(application))
  val substContextView =
    new TreeView[Expression[IndexType]] {
      root = substContextRoot
      showRoot = false
      cellFactory = (_ => new SubstitutionContextTreeCell)
    }

  def buildSubstitutionContext(expr : Expression[IndexType]) : TreeItem[Expression[IndexType]] = 
    expr match {
      case app @ Application(defn, _, _) => {
        val appItem = new TreeItem[Expression[IndexType]] { value = app }

        defn.contextBuffer foreach (e => {
          appItem.children += buildSubstitutionContext(promoteInternalExpr(e)).delegate
        })

        appItem
      }
      case other @ _ => new TreeItem[Expression[IndexType]] { value = other }
    }

  substContextView.getSelectionModel.selectedItem onChange {
    val exprItem = substContextView.getSelectionModel.selectedItem()

    if (exprItem != null) {
      // generate a preview and set it in the window ...

    } else {
    }

    contextView.getSelectionModel.clearSelection
  }

  // contextView.getSelectionModel.selectedItem onChange {
  //   val exprItem = contextView.getSelectionModel.selectedItem()

  //   if (exprItem != null) {
  //     val expr = exprItem.value()
  //     activeExpression = Some(expr)
  //     activeExpressionIndex = indexOf(contextRoot, exprItem)

  //     // Now make a gallery and show it in the preview pane
  //     val gallery = new ShapeGallery(expr)
  //     editor.setPreviewGallery(gallery)
  //   } else {
  //     activeExpression = None
  //     activeExpressionIndex = Seq.empty
  //   }
  // }

  class SubstitutionContextTreeCell extends jfxsc.TreeCell[Expression[SubstIndex]] {

    getStyleClass add "orch-list-cell"
    val cellStyleIndex = getStyleClass.length
    getStyleClass add "orch-list-null"

    def setCellStyleType(str : String) = {
      getStyleClass(cellStyleIndex) = str
    }

    override def updateItem(expr : Expression[SubstIndex], empty : Boolean) = {
      super.updateItem(expr, empty)

      if (! empty) {
        setCellStyleType("orch-list-cell-" ++ expr.styleString)
        setText(context.expandIdentifier(expr.ident))
      } else {
        setCellStyleType("orch-list-null")
        setText("")
      }
    } 
  }

  def newGallery(seed : NCell[Polarity[IndexType]]) : WorksheetGallery = 
    new SubstitutionWorksheetGallery(seed)

  class SubstitutionWorksheetPanel(val complex : SubstitutionWorksheet, val baseIndex : Int) extends WorksheetPanel { thisPanel =>

    type CellType = SubstitutionWorksheetPanelCell
    type EdgeType = SubstitutionWorksheetPanelEdge

    type ComplexType = SubstitutionWorksheet

    def newCell(owner : complex.SubstitutionWorksheetCell) : SubstitutionWorksheetPanelCell = {
      val cell = new SubstitutionWorksheetPanelCell(owner)
      owner.registerPanelCell(thisPanel)(cell)
      reactTo(cell)
      cell
    }
    
    def newEdge(owner : complex.SubstitutionWorksheetCell) : SubstitutionWorksheetPanelEdge = {
      val edge = new SubstitutionWorksheetPanelEdge(owner)
      owner.registerPanelEdge(thisPanel)(edge)
      reactTo(edge)
      edge
    }
    
    class SubstitutionWorksheetPanelCell(val owner : complex.SubstitutionWorksheetCell) extends WorksheetPanelCell
    class SubstitutionWorksheetPanelEdge(val owner : complex.SubstitutionWorksheetCell) extends WorksheetPanelEdge

    //============================================================================================
    // INITIALIZATION
    //

    var baseCell : SubstitutionWorksheetPanelCell = newCell(complex.baseCells(baseIndex))

    refreshPanelData
    initializeChildren

  }

  class SubstitutionWorksheetGallery(val complex : SubstitutionWorksheet) extends WorksheetGallery {

    type PanelType = SubstitutionWorksheetPanel

    def this(seed : NCell[Polarity[IndexType]]) = this(new SubstitutionWorksheet(seed))

    def newPanel(i : Int) : SubstitutionWorksheetPanel = {
      val panel = new SubstitutionWorksheetPanel(complex, i)
      reactTo(panel)
      panel
    }

    initialize

  }

}
