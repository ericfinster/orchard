/**
  * CardinalPanel.scala - A Panel for displaying cardinals
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import javafx.scene.paint.Color

import orchard.core._

class CardinalPanel[A](val complex : CardinalComplex[A], baseIndex : Int) extends JavaFXPanel[Polarity[A]] with MutablePanel[Polarity[A]] {

  type CellType = CardinalCell
  type EdgeType = CardinalEdge

  type ComplexType = CardinalComplex[A]

  class CardinalCell(val owner : complex.CardinalCell) extends JavaFXCell with MutablePanelCell {

    if (owner.isPolarized)
      getStyleClass.add("cardinal-cell-polarized")
    else
      getStyleClass.add("cardinal-cell-neutral")

    // This is awkward because of your event structure right now.  See the discussion in
    // Events.scala ....
    override def onEventEmitted(ev : CellEvent) = {
      if (ev.isInstanceOf[complex.ChangeEvents.ChangeEvent]) {
        onCellChangeEvent(ev.asInstanceOf[complex.ChangeEvents.ChangeEvent])
      } else {
        ev match {
          case CellEntered(cell) => owner.emitToFaces(RequestHovered)
          case CellExited(cell) => owner.emitToFaces(RequestUnhovered)
          case RequestSelected => doSelect
          case RequestDeselected => doDeselect
          case RequestHovered => doHover
          case RequestUnhovered => doUnhover
          case _ => ()
        }
      }
    }

    def doHover = getStyleClass.add(if (owner.isPolarized) "cardinal-polarized-hover" else "cardinal-neutral-hover")
    def doSelect = getStyleClass.add(if (owner.isPolarized) "cardinal-polarized-selected" else "cardinal-neutral-selected")
    def doUnhover = getStyleClass.remove(if (owner.isPolarized) "cardinal-polarized-hover" else "cardinal-neutral-hover")
    def doDeselect = getStyleClass.remove(if (owner.isPolarized) "cardinal-polarized-selected" else "cardinal-neutral-selected")

  }

  class CardinalEdge(val owner : complex.CardinalCell) extends JavaFXEdge with MutablePanelEdge {
    override def onEventEmitted(ev : CellEvent) = {
      ev match {
        case RequestSelected => setStroke(Color.RED)
        case RequestDeselected => setStroke(Color.BLACK)
        case RequestHovered => setStroke(Color.RED)
        case RequestUnhovered => setStroke(Color.BLACK)
        case _ => ()
      }

    super.onEventEmitted(ev)
    }
  }

  def newCell(owner : complex.CardinalCell) : CardinalCell = { val cell = new CardinalCell(owner) ; reactTo(cell) ; cell }
  def newEdge(owner : complex.CardinalCell) : CardinalEdge = { val edge = new CardinalEdge(owner) ; reactTo(edge) ; edge }

  //============================================================================================
  // UI INITIALIZATION
  //

  var baseCell : CardinalCell = {
    val seed = complex.baseCells(baseIndex)
    generatePanelData(seed, for { srcs <- seed.sources } yield (srcs map (src => newEdge(src))))
  }

  initializeChildren

}
