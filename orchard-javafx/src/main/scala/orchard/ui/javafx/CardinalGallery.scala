/**
  * CardinalGallery.scala - A Gallery of cardinal panels
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

import javafx.scene.layout.HBox

import orchard.core._

class CardinalGallery[A](seed : NCell[Polarity[A]]) extends JavaFXGallery[Polarity[A]] {

  type PanelType = CardinalPanel[A]

  val complex : CardinalComplex[A] = new CardinalComplex(seed)
  reactTo(complex)

  private val myPanels = new ListBuffer[CardinalPanel[A]]
  myPanels ++= { for { i <- Range(0, complex.baseCells.length) } yield { newPanel(i) } }

  def panels : List[PanelType] = myPanels.toList
  def newPanel(i : Int) : CardinalPanel[A] = { val panel = new CardinalPanel(complex, i) ; reactTo(panel) ; panel }

  hbox.getChildren.addAll(panels)

  override def onEventEmitted(ev : CellEvent) = {
    ev match {
      case ComplexExtended => {
        val extPanel = newPanel(complex.baseCells.length - 1)
        myPanels += extPanel
        hbox.getChildren.add(extPanel)
        extPanel.render
        fastForward
      }
      case _ => ()
    }

    // Pass along the event to any listeners ...
    emit(ev)
  }
}
