/**
  * JavaFXModuleUI.scala - User Interface routines for modules
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.Includes._
import scalafx.scene.text._
import scalafx.scene.layout._
import scalafx.scene.control._

import scalafx.geometry._

import javafx.{scene => jfxs}

import orchard.core.cell._
import orchard.core.complex._
import orchard.core.expression._

trait JavaFXModuleUI { thisModule : JavaFXModule =>

  val worksheetTabPane = new TabPane

  val worksheetPane = new StackPane {
    padding = Insets(10, 10, 10, 10)
    styleClass += "orch-pane"
    content = worksheetTabPane
  }

  val controlTabPane = new TabPane {
    side = Side.BOTTOM
  }

  val controlPane = new StackPane {
    padding = Insets(10, 10, 10, 10)
    styleClass += "orch-pane"
    content = controlTabPane
  }

  val clipboardPane = new StackPane {
    padding = Insets(10, 10, 10, 10)
    styleClass += "orch-pane"
  }

  val clipboardTab = new Tab {
    text = "Clipboard"
    content = clipboardPane
  }

  controlTabPane += clipboardTab

  val moduleSplit = new SplitPane {
    orientation = Orientation.VERTICAL
    items ++= List(worksheetPane, controlPane)
    dividerPositions = 0.6f
  }

  val ui = moduleSplit

  //============================================================================================
  // CLIPBOARD MANIPULATION
  //

  private var theClipboardExpression : Option[Expression] = None

  def clipboardExpression : Option[Expression] = theClipboardExpression
  def clipboardExpression_=(exprOpt : Option[Expression]) = {
    theClipboardExpression = exprOpt

    exprOpt match {
      case None => clipboardPane.content.clear
      case Some(expr) => {
        val gallery = new FrameworkGallery(expr.ncell map (Some(_)))
        clipboardPane.content = gallery
        gallery.refreshAll
      }
    }
  }

  //============================================================================================
  // WORKSHEET MANIPULATION
  //

  var activeGallery : Option[WorksheetGallery] = None
  var sheetCount : Int = 1

  def newSheet : Unit = newSheet(CardinalComplex(Object(None)))
  // def newSheetWithExpression(ncell : NCell[Expression]) = newSheet(CardinalComplex(ncell map (Some(_))))

  def newSheet(seed : NCell[Polarity[Option[Expression]]]) : Unit = {
    val worksheet = new Worksheet(seed)
    worksheets += worksheet
    displayWorksheet(worksheet)
  }

  def newSheet(expr : Expression) : Unit =
    newSheet(CardinalComplex(expr.ncell map (Some(_))))

  def displayWorksheet(worksheet : Worksheet) : Unit = {
    val gallery = new WorksheetGallery(worksheet)

    val tab = new Tab {
      text = "Sheet " ++ sheetCount.toString
      content = gallery

      onClosed = () => {
        worksheets -= gallery.complex
      }

      onSelectionChanged = () => {
        if (selected())
          activeGallery = Some(gallery)
      }
    }

    worksheetTabPane += tab
    worksheetTabPane.selectionModel().select(tab)
    sheetCount += 1
    gallery.refreshAll
  }

  //============================================================================================
  // WORKSHEET PANEL IMPLEMENTATION
  //

  class WorksheetPanel(val complex : Worksheet, val baseIndex : Int) 
      extends ZoomPanel[Polarity[Option[Expression]]] { thisPanel =>

    type ComplexType = Worksheet

    type CellType = WorksheetPanelCell
    type EdgeType = WorksheetPanelEdge

    def newCell(owner : complex.WorksheetCell) : WorksheetPanelCell =
      new WorksheetPanelCell(owner)
    
    def newEdge(owner : complex.WorksheetCell) : WorksheetPanelEdge =
      new WorksheetPanelEdge(owner)

    class WorksheetPanelCell(val owner : complex.WorksheetCell) extends JavaFXCell { thisCell : CellType =>

      def renderLabel : jfxs.Node = {
        val labelNode = 
          item match {
            case Positive => new Text("+")
            case Negative => new Text("-")
            case Neutral(None) => new Region { prefWidth = 10 ; prefHeight = 10 }
            case Neutral(Some(expr)) => new Text(expr.id)
          }

        labelNode.layoutBounds onChange { thisPanel.refresh }
        pane.getChildren.setAll(labelNode)
        labelNode
      }

      def getStyleString =
        item match {
          case Positive => "polarized"
          case Negative => "polarized"
          case Neutral(None) => if (owner.isFillable) "exposed" else "empty"
          case Neutral(Some(expr)) => expr.styleString
        }

      override def onEventEmitted(ev : CellEvent) = {
        ev match {
          case complex.ChangeEvents.ItemChangedEvent(oldItem) => { renderCell ; super.onEventEmitted(ev) }
          case CellEntered(cell) => if (owner.isPolarized) () else { owner.emitToFaces(RequestCellHovered) ; owner.emit(RequestEdgeHovered) }
          case CellExited(cell) => if (owner.isPolarized) () else { owner.emitToFaces(RequestCellUnhovered) ; owner.emit(RequestEdgeUnhovered) }
          case _ => super.onEventEmitted(ev)
        }
      }

      renderCell

      owner.registerPanelCell(thisPanel)(thisCell)
      thisPanel.reactTo(thisCell)

    }

    class WorksheetPanelEdge(val owner : complex.WorksheetCell) extends JavaFXEdge { thisEdge : EdgeType => 

      owner.registerPanelEdge(thisPanel)(thisEdge)
      thisPanel.reactTo(thisEdge)
    
    }

    //============================================================================================
    // INITIALIZATION
    //

    var baseCell : WorksheetPanelCell = newCell(complex.baseCells(baseIndex))

    refreshPanelData
    initializeChildren

  }

  //============================================================================================
  // WORKSHEET GALLERY IMPLEMENTATION
  //

  class WorksheetGallery(val complex : Worksheet) extends SpinnerGallery[Polarity[Option[Expression]]] { thisGallery =>

    def this(seed : NCell[Polarity[Option[Expression]]]) = this(new Worksheet(seed))

    type PanelType = WorksheetPanel

    def newPanel(i : Int) : WorksheetPanel = {
      val panel = new WorksheetPanel(complex, i)
      reactTo(panel)
      panel
    }

    initialize

    override def refreshAll = {
      super.refreshAll
      panels foreach (panel => {
        panel.baseCell foreachCell (cell => cell.assignStyle)
      })
    }

    //============================================================================================
    // EVENTS
    //

    override def onEventEmitted(ev : CellEvent) = {
      val cmplx = complex

      ev match {

        case PanelClicked => {
          cmplx.deselectAll
        }

        case ComplexExtended => {
          this(cmplx.dimension - 1).refresh
          val extPanel = newPanel(cmplx.dimension)
          appendPanel(extPanel)
          extPanel.render
          fastForward
        }

        case CellClicked(c) => {
          val cell = c.owner.asInstanceOf[cmplx.CellType]

          if (cell.isNeutral) {
            cmplx.clearAndSelect(cell)
          } else {
            cmplx.deselectAll
          }
        }

        case CellDoubleClicked(c) => {
          val cell = c.owner.asInstanceOf[cmplx.CellType]

          if (cell.isNeutral) {
            newSheet(CardinalComplex(cell.neutralNCell))
          }
        }

        case CellCtrlClicked(c) => {
          val cell = c.owner.asInstanceOf[cmplx.CellType]

          cmplx.selectionBase match {
            case None => if (cell.isNeutral) cmplx.selectAsBase(cell)
            case Some(base) => {
              if (cell != base) {
                if (cell.isPolarized) {
                  cmplx.deselectAll
                } else {
                  if (! cmplx.trySelect(cell)) cmplx.clearAndSelect(cell)
                }
              }
            }
          }
        }

        // This is clearly overkill, but the nooks need to be recalculated ...
        case cmplx.ChangeEvents.ItemChangedEvent(oldItem) => {
          refreshAll
        }

        case cmplx.ChangeEvents.CompositeInsertionEvent(c, u) => {
          val dim = c.dimension

          val compPanel = thisGallery(dim)
          val univPanel = thisGallery(dim + 1)

          val affectedDimensions = Range(dim, cmplx.dimension + 1)

          affectedDimensions foreach (i => panels(i).refresh)
        }

        case _ => super.onEventEmitted(ev)
      }
    }
  }

}
