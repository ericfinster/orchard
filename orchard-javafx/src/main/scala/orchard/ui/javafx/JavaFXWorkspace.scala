/**
  * JavaFXWorkspace.scala - Base trait for workspaces in JavaFX
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scala.collection.JavaConversions._

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

sealed trait NavigationTreeItem
case class DefnWorkspaceItem(val wksp : JavaFXDefinitionWorkspace) extends NavigationTreeItem
case class SubstWorkspaceItem(val wksp : JavaFXSubstitutionWorkspace) extends NavigationTreeItem

trait JavaFXWorkspace extends Workspace {

  type EnvironmentSeqType = ObservableBuffer[NCell[Expression]]

  val environment = ObservableBuffer.empty[NCell[Expression]]

  // I think I'm going to move the environment here into the workspace definition.
  val environmentRoot = new TreeItem[NCell[Expression]]
  val environmentView =
    new TreeView[NCell[Expression]] {
      root = environmentRoot
      showRoot = false
      cellFactory = (_ => new EnvironmentTreeCell)
    }

  environmentView.getSelectionModel.selectedItem onChange {
    activeExpression = Some(environmentView.getSelectionModel.selectedItem().value())
  }

  // Use the actual changes, smart guy ...
  environment onChange { (_, chgs) =>  
    chgs foreach {
      case Add(pos, added) => {
        val items =
          added.asInstanceOf[Traversable[NCell[Expression]]] map 
            (expr => new TreeItem[NCell[Expression]] { value = expr }.delegate)

        environmentRoot.children.insertAll(pos, items)
      }
      case Remove(pos, removed) => ???
      case Reorder(_, _, _) => ???
    }
  }

  class EnvironmentTreeCell extends jfxsc.TreeCell[NCell[Expression]] {

    getStyleClass add "orch-list-cell"
    val cellStyleIndex = getStyleClass.length
    getStyleClass add "orch-list-null"

    def setCellStyleType(str : String) = {
      getStyleClass(cellStyleIndex) = str
    }

    override def updateItem(expr : NCell[Expression], empty : Boolean) = {
      super.updateItem(expr, empty)

      if (! empty) {
        // Set the style based on the semantics ...
        expr.value match {
          case Variable(_, isThin) => {
            if (isThin) {
              setCellStyleType("orch-list-cell-var-thin")
            } else {
              setCellStyleType("orch-list-cell-var")
            }
          }
          case Filler(_) => setCellStyleType("orch-list-cell-filler")
          case FillerFace(_, _, isThin) => {
            if (isThin) {
              setCellStyleType("orch-list-cell-filler-face-thin")
            } else {
              setCellStyleType("orch-list-cell-filler-face")
            }
          }
          case UnicityFiller(_) => setCellStyleType("orch-list-cell-ufiller")
          case Application(_, _, _) => setCellStyleType("orch-list-cell-app")
          // Should look up the cell and use that style type ...
          case Projection(_) => setCellStyleType("orch-list-cell-filler")
        }

        setText(expr.toString)
      } else {
        setCellStyleType("orch-list-null")
        setText("")
      }
    } 
  }

  def treeItem : TreeItem[NavigationTreeItem]

  var sheetCount : Int = 1

  val sheetTabPane = new TabPane {
    side = Side.TOP
  }

  var activeGallery : Option[WorksheetGallery] = None 
  var activeExpression : Option[NCell[Expression]] = None

  def activeSheet : Option[Worksheet] =
    for {
      gallery <- activeGallery
    } yield gallery.complex

  def newSheet = newSheet(Object(None))

  def newSheet(seed : NCell[Option[Expression]]) : Unit =
    newCardinalSheet(CardinalComplex(seed))

  def newCardinalSheet(seed : NCell[Polarity[Option[Expression]]]) : Unit = {
    val gallery = new WorksheetGallery(seed)

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

  def apply(defn : Definition) = applyInShell(Object(None), defn)

  def applyInShell(shell : NCell[Option[Expression]], defn : Definition) : Option[JavaFXSubstitutionWorkspace] = {
    val substWksp =
      new JavaFXSubstitutionWorkspace(
        editor.asInstanceOf[JavaFXEditor],
        defn.name,
        this,
        defn,
        shell
      )

    // Import the current environment
    substWksp.environment ++= environment

    // Copy the sheets
    sheets foreach (sheet => {
      substWksp.newCardinalSheet(sheet.toCell)
    })

    // Add the new workspace as a child
    treeItem.children += substWksp.treeItem

    Some(substWksp)
  }

  def unfoldSelectedApplication : Unit = {
    val selectedItem = environmentView.getSelectionModel.selectedItem()

    if (selectedItem != null) {
      val selectedExpr = selectedItem.value()

      selectedExpr.value match {
        case a @ Application(_, _, _) => {
          println("About to unfold...")

          val exprs = unfold(a)

          exprs foreach (expr => {
            println("Found resulting expression: " ++ expr.value.id)
            selectedItem.children += new TreeItem[NCell[Expression]] { value = expr }.delegate
          })

        }
        case _ => println("Not an application!")
      }
    }
  }

  override def unfold(app : Application) : Seq[NCell[Expression]] = {
    val exprs = super.unfold(app)

    exprs
  }

  class WorksheetPanel(val complex : Worksheet, val baseIndex : Int) extends JavaFXWorksheetPanel { thisPanel =>

    type CellType = WorksheetCell
    type EdgeType = WorksheetEdge

    type ComplexType = Worksheet

    class WorksheetCell(val owner : complex.WorksheetCell) extends JavaFXWorksheetCell
    class WorksheetEdge(val owner : complex.WorksheetCell) extends JavaFXWorksheetEdge

    def newCell(owner : complex.WorksheetCell) : WorksheetCell = {
      val cell = new WorksheetCell(owner)
      owner.registerPanelCell(thisPanel)(cell)
      reactTo(cell)
      cell
    }
    
    def newEdge(owner : complex.WorksheetCell) : WorksheetEdge = {
      val edge = new WorksheetEdge(owner)
      owner.registerPanelEdge(thisPanel)(edge)
      reactTo(edge)
      edge
    }

    //============================================================================================
    // INITIALIZATION
    //

    var baseCell : WorksheetCell = newCell(complex.baseCells(baseIndex))

    refreshPanelData
    initializeChildren

  }

  class WorksheetGallery(val complex : Worksheet) extends JavaFXWorksheetGallery {

    def this(seed : NCell[Polarity[Option[Expression]]]) = this(new Worksheet(seed))
    def this() = this(Composite(Negative, Seed(Object(Neutral(None))), Positive))

    type PanelType = WorksheetPanel

    def newPanel(i : Int) : WorksheetPanel = {
      val panel = new WorksheetPanel(complex, i)
      reactTo(panel)
      panel
    }

    initialize
  }

}

class JavaFXDefinitionWorkspace(
  val editor : JavaFXEditor,
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
  val editor : JavaFXEditor,
  val name : String,
  val parentWorkspace : Workspace,
  val defn : Definition,
  val shell : NCell[Option[Expression]]
) extends SubstitutionWorkspace with JavaFXWorkspace { thisWksp =>

  type GoalSeqType = ObservableBuffer[GoalComplex]

  val goals = ObservableBuffer.empty[GoalComplex]

  val treeItem = new TreeItem[NavigationTreeItem] {
    value = SubstWorkspaceItem(thisWksp)
  }

  def stabilityLevel : Option[Int] = parentWorkspace.stabilityLevel
  def invertibilityLevel : Option[Int] = parentWorkspace.invertibilityLevel
  def unicityLevel : Option[Int] = parentWorkspace.unicityLevel

  // We need a panel and gallery class to display the goals, since these are stored in a different
  // kind of complex.  And really, the inheritence hierarchy needs to be examined to make if a little
  // cleaner

  initialize

  class GoalPanel(val complex : GoalComplex, val baseIndex : Int) 
      extends ZoomPanel[GoalMarker] 
      with MutablePanel[GoalMarker] { thisPanel =>

    type CellType = GoalCell
    type EdgeType = GoalEdge

    type ComplexType = GoalComplex

    class GoalCell(val owner : complex.GoalComplexCell) extends JavaFXCell with MutablePanelCell {

      def renderLabel : jfxs.Node = {
        val labelNode = new Text(item.toString)
          // item match {
          //   case Positive => new Text("+")
          //   case Negative => new Text("-")
          //   case Neutral(None) => new Region { prefWidth = 10 ; prefHeight = 10 }
          //   case Neutral(Some(expr)) => new Text(expr.id)
          // }

        labelNode.layoutBounds onChange { thisPanel.refresh }
        pane.getChildren.setAll(labelNode)
        labelNode
      }

    }

    class GoalEdge(val owner : complex.GoalComplexCell) extends JavaFXEdge with MutablePanelEdge

    def newCell(owner : complex.GoalComplexCell) : GoalCell = {
      val cell = new GoalCell(owner)
      owner.registerPanelCell(thisPanel)(cell)
      reactTo(cell)
      cell
    }

    def newEdge(owner : complex.GoalComplexCell) : GoalEdge = {
      val edge = new GoalEdge(owner)
      owner.registerPanelEdge(thisPanel)(edge)
      reactTo(edge)
      edge
    }

    //============================================================================================
    // INITIALIZATION
    //

    var baseCell : GoalCell = newCell(complex.baseCells(baseIndex))

    refreshPanelData
    initializeChildren

  }

  class GoalGallery(val complex : GoalComplex) extends SpinnerGallery[GoalMarker] { thisGallery =>

    type PanelType = GoalPanel

    def newPanel(i : Int) : GoalPanel = {
      val panel = new GoalPanel(complex, i)
      reactTo(panel)
      panel
    }

    initialize
  }
}
