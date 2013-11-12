/**
  * CardinalPanel.scala - A Panel for displaying cardinals
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import javafx.scene.Node
import javafx.scene.text.Text
import javafx.scene.paint.Color

import orchard.core._

class CardinalPanel[A](val complex : SimpleCardinalComplex[A], baseIndex : Int) extends JavaFXPanel[Polarity[A]] with MutablePanel[Polarity[A]] {

  type CellType = CardinalCell
  type EdgeType = CardinalEdge

  type ComplexType = SimpleCardinalComplex[A]

  class CardinalCell(owner : complex.SimpleCardinalCell) extends JavaFXCell(owner) with MutablePanelCell {

    // This should be more complicated.  You should use the renderability of A somehow.  I guess
    // I'll experiment with this in the expression class

    def renderLabel = new Text(item.toString)

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
        super.onEventEmitted(ev)
      }
    }

    override def doHover = getStyleClass.add(if (owner.isPolarized) "cardinal-polarized-hover" else "cardinal-neutral-hover")
    override def doSelect = getStyleClass.add(if (owner.isPolarized) "cardinal-polarized-selected" else "cardinal-neutral-selected")
    override def doUnhover = getStyleClass.remove(if (owner.isPolarized) "cardinal-polarized-hover" else "cardinal-neutral-hover")
    override def doDeselect = getStyleClass.remove(if (owner.isPolarized) "cardinal-polarized-selected" else "cardinal-neutral-selected")

  }

  class CardinalEdge(owner : complex.SimpleCardinalCell) extends JavaFXEdge(owner) with MutablePanelEdge {

    override def doHover : Unit = setStroke(Color.RED)
    override def doSelect : Unit = setStroke(Color.RED)
    override def doUnhover : Unit = setStroke(Color.BLACK)
    override def doDeselect : Unit = setStroke(Color.BLACK)

  }

  def newCell(owner : complex.SimpleCardinalCell) : CardinalCell = { val cell = new CardinalCell(owner) ; reactTo(cell) ; cell }
  def newEdge(owner : complex.SimpleCardinalCell) : CardinalEdge = { val edge = new CardinalEdge(owner) ; reactTo(edge) ; edge }

  //============================================================================================
  // UI INITIALIZATION
  //

  var baseCell : CardinalCell = {
    val seed = complex.baseCells(baseIndex)
    generatePanelData(seed, for { srcs <- seed.sources } yield (srcs map (src => newEdge(src))))
  }

  initializeChildren

}
