/**
  * ExpressionBuilder.scala
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

import orchard.core._

import Util._

class ExpressionBuilder(seed : NCell[Polarity[Option[Expression]]]) extends SpinnerGallery[Polarity[Option[Expression]]] with ExpressionGallery { thisBuilder =>

  def this() = this(Composite(Negative, Seed(Object(Neutral(None))), Positive))

  //============================================================================================
  // INITIALIZATION
  //

  type PanelType = ExpressionBuilderPanel

  val complex = new ExpressionBuilderComplex(seed)

  reactTo(complex)

  def newPanel(i : Int) : ExpressionBuilderPanel = {
    val panel = new ExpressionBuilderPanel(complex, i)
    reactTo(panel) 
    panel 
  }

  initialize

  var lastComposite : GalleryCell = null
  var lastFiller : GalleryCell = null

  //============================================================================================
  // EVENTS
  //

  override def onEventEmitted(ev : CellEvent) = {
    ev match {

      case PanelClicked => {
        deselectAll
      }

      case ComplexExtended => {
        this(complex.dimension - 1).refresh
        val extPanel = newPanel(complex.dimension)
        appendPanel(extPanel)
        extPanel.render
        fastForward
      }

      case CellClicked(c) => {
        val cell = c.asInstanceOf[GalleryCell]

        if (cell.owner.isNeutral)
          clearAndSelect(cell)
        else {
          deselectAll
        }
      }

      case CellCtrlClicked(c) => {
        val cell = c.asInstanceOf[GalleryCell]

        selectionBase match {
          case None => if (cell.owner.isNeutral) selectAsBase(cell)
          case Some(base) => {
            if (cell != base) {
              if (cell.owner.isPolarized) {
                deselectAll
              } else {
                if (!trySelect(cell)) clearAndSelect(cell)
              }
            }
          }
        }
      }

      case complex.ChangeEvents.CompositeInsertionEvent(c, u) => {
        val dim = c.dimension

        val compPanel = thisBuilder(dim)
        val univPanel = thisBuilder(dim + 1)

        lastComposite = c.cellOnPanel(compPanel)
        lastFiller = u.cellOnPanel(univPanel)

        val affectedDimensions = Range(dim, complex.dimension + 1)

        affectedDimensions foreach (i => panels(i).refresh)
      }

      case _ => super.onEventEmitted(ev)
    }
  }
}
