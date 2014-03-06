/**
  * JavaFXWorksheetGallery.scala - JavaFXImplementation of an expression gallery
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import orchard.core._

abstract class JavaFXWorksheetGallery extends SpinnerGallery[Polarity[Option[Expression]]] { thisGallery =>

  type PanelType <: JavaFXWorksheetPanel
  
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
        val cell = c.owner.asInstanceOf[cmplx.WorksheetCell]

        if (cell.isNeutral) {
          cmplx.clearAndSelect(cell)
        } else {
          cmplx.deselectAll
        }
      }

      case CellCtrlClicked(c) => {
        val cell = c.owner.asInstanceOf[cmplx.WorksheetCell]

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

      case cmplx.ChangeEvents.ItemChangedEvent(oldItem) => {
        refreshAll  // Probably don't need all ...
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
