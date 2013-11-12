/**
  * SimpleCardinalComplex.scala - A Simple implementation of a cardinal complex
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

class SimpleCardinalComplex[A](seed : NCell[Polarity[A]]) extends MutableComplex[Polarity[A]] with CardinalComplex[A] {

  type CellType = SimpleCardinalCell

  def newCell(item : Polarity[A]) = new SimpleCardinalCell(item)
  def extend = glob(Negative, Positive)

  populateComplex(seed)

  class SimpleCardinalCell(var item : Polarity[A]) extends MutableCell with CardinalCell 

}
