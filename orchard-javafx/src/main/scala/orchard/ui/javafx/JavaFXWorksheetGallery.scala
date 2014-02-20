/**
  * JavaFXWorksheetGallery.scala - JavaFXImplementation of an expression gallery
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import orchard.core._

class JavaFXWorksheetGallery(cmplx : ExpressionWorksheet) extends SpinnerGallery[Polarity[Option[Expression]]] { thisGallery =>

  def this(seed : NCell[Polarity[Option[Expression]]]) = this(new ExpressionWorksheet(seed))
  def this() = this(Composite(Negative, Seed(Object(Neutral(None))), Positive))

  //============================================================================================
  // INITIALIZATION
  //

  type PanelType = JavaFXWorksheetPanel
  
  val complex = cmplx

  def newPanel(i : Int) : JavaFXWorksheetPanel = {
    val panel = new JavaFXWorksheetPanel(complex, i)
    reactTo(panel) 
    panel 
  }

  initialize

  //============================================================================================
  // EVENTS
  //

  override def onEventEmitted(ev : CellEvent) = {
    ev match {

      case PanelClicked => {
        complex.deselectAll
      }

      case ComplexExtended => {
        this(complex.dimension - 1).refresh
        val extPanel = newPanel(complex.dimension)
        appendPanel(extPanel)
        extPanel.render
        fastForward
      }

      case CellClicked(c) => {
        val cell = c.owner.asInstanceOf[complex.ExpressionWorksheetCell]

        if (cell.isNeutral) {
          complex.clearAndSelect(cell)
        } else {
          complex.deselectAll
        }
      }

      case CellCtrlClicked(c) => {
        val cell = c.owner.asInstanceOf[complex.ExpressionWorksheetCell]

        complex.selectionBase match {
          case None => if (cell.isNeutral) complex.selectAsBase(cell)
          case Some(base) => {
            if (cell != base) {
              if (cell.isPolarized) {
                complex.deselectAll
              } else {
                if (! complex.trySelect(cell)) complex.clearAndSelect(cell)
              }
            }
          }
        }
      }

      case complex.ChangeEvents.ItemChangedEvent(oldItem) => {
        refreshAll  // Probably don't need all ...
      }

      case complex.ChangeEvents.CompositeInsertionEvent(c, u) => {
        val dim = c.dimension

        val compPanel = thisGallery(dim)
        val univPanel = thisGallery(dim + 1)

        val affectedDimensions = Range(dim, complex.dimension + 1)

        affectedDimensions foreach (i => panels(i).refresh)
      }

      case _ => super.onEventEmitted(ev)
    }
  }
}
