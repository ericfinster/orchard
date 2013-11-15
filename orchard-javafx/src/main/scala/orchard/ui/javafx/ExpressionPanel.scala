/**
  * ExpressionPanel.scala - A simple panel for rendering expressions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import orchard.core._

import javafx.scene.Node
import javafx.scene.text.Text

class ExpressionPanel(val complex : ExpressionComplex, baseIndex : Int) extends JavaFXPanel[Expression] { thisPanel =>

  type CellType = ExpressionCell
  type EdgeType = ExpressionEdge

  type ComplexType = ExpressionComplex

  def newCell(owner : complex.CellType) = new ExpressionCell(owner)
  def newEdge(owner : complex.CellType) = new ExpressionEdge(owner)

  class ExpressionCell(owner : complex.CellType) extends JavaFXCell(owner) {

    def renderLabel : Node = {
      val labelNode = new Text(item.id)

      pane.getChildren.setAll(labelNode)
      labelNode
    }

  }

  class ExpressionEdge(owner : complex.CellType) extends JavaFXEdge(owner)

  //============================================================================================
  // INITIALIZATION
  //

  var baseCell : ExpressionCell = {
    val seed = complex.baseCells(baseIndex)
    generatePanelData(seed, for { srcs <- seed.sources } yield (srcs map (src => newEdge(src))))
  }

  initializeChildren

}
