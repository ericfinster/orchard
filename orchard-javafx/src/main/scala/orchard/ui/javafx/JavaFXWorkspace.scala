/**
  * JavaFXWorkspace.scala - Base trait for workspaces in JavaFX
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.Includes._

import scalafx.geometry._

import scalafx.scene.layout._
import scalafx.scene.control._

import scalafx.collections.ObservableBuffer

import javafx.{scene => jfxs}
import javafx.scene.text.Text

import orchard.core._

sealed trait NavigationTreeItem
case class DefnWorkspaceItem(val wksp : JavaFXDefinitionWorkspace) extends NavigationTreeItem
case class SubstWorkspaceItem(val wksp : JavaFXSubstitutionWorkspace) extends NavigationTreeItem

trait JavaFXWorkspace extends Workspace {

  type EnvironmentSeqType = ObservableBuffer[NCell[Expression]]

  def treeItem : TreeItem[NavigationTreeItem]

  var sheetCount : Int = 1

  val sheetTabPane = new TabPane {
    side = Side.TOP
  }

  val environment = ObservableBuffer.empty[NCell[Expression]]

  var activeGallery : Option[WorksheetGallery] = None 
  var activeExpression : Option[NCell[Expression]] = None

  def activeSheet : Option[Worksheet] =
    for {
      gallery <- activeGallery
    } yield gallery.complex

  def newSheet = newSheet(Object(None))

  def newSheet(seed : NCell[Option[Expression]]) : Unit = {
    val gallery = new WorksheetGallery(CardinalComplex(seed))

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
