/**
  * SimplePanel.scala - A Simple Panel Implementation
  * 
  * @author Eric Finster
  * @version 0.1
  */

package orchard.ui.javafx

import javafx.scene.Node
import javafx.scene.text.Text

import orchard.core._

class SimplePanel[A](val complex : SimpleMutableComplex[A], baseIndex : Int) extends JavaFXPanel[A] {

  type CellType = SimpleCell
  type EdgeType = SimpleEdge

  type ComplexType = SimpleMutableComplex[A]

  class SimpleCell(owner : complex.SimpleMutableCell) extends JavaFXCell(owner) {

    def renderLabel : Node = new Text(item.toString)

  }

  class SimpleEdge(owner : complex.SimpleMutableCell) extends JavaFXEdge(owner)

  def newCell(owner : complex.SimpleMutableCell) : SimpleCell = new SimpleCell(owner)
  def newEdge(owner : complex.SimpleMutableCell) : SimpleEdge = new SimpleEdge(owner)

  //============================================================================================
  // UI INITIALIZATION
  //

  override val baseCell : SimpleCell = {
    val seed = complex.baseCells(baseIndex)
    generatePanelData(seed, for { srcs <- seed.sources } yield (srcs map (src => newEdge(src))))
  }

  initializeChildren

}
