/**
  * SimpleCardinalComplex.scala - A Simple implementation of a cardinal complex
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.complex

import orchard.core.cell._

class SimpleCardinalComplex[A](seed : NCell[Polarity[A]]) extends AbstractMutableComplex[Polarity[A]](seed) with CardinalComplex[A] {

  type CellType = SimpleCardinalCell

  def newCell(item : Polarity[A]) = new SimpleCardinalCell(item)
  def extend = glob(Negative, Positive)

  class SimpleCardinalCell(var item : Polarity[A]) extends AbstractMutableCell with CardinalCell 

}
