/**
  * ExpressionBuilderComplex.scala - A complex which holds incomplete expressions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

class ExpressionBuilderComplex(seed : NCell[Polarity[Option[Expression]]])
    extends MutableComplex[Polarity[Option[Expression]]]
    with CardinalComplex[Option[Expression]] 
    with ExpressionFramework[Polarity[Option[Expression]]] {

  type CellType = ExpressionBuilderCell

  // Initialize from the seed ...
  populateComplex(seed)

  def newCell(expr : Polarity[Option[Expression]]) = new ExpressionBuilderCell(expr)
  def extend = glob(Negative, Positive)

  class ExpressionBuilderCell(initialItem : Polarity[Option[Expression]]) 
      extends MutableCell 
      with ExpressionFrameworkCell
      with CardinalCell { thisCell =>

    private var myItem : Polarity[Option[Expression]] = initialItem

    def item = myItem
    def item_=(newItem : Polarity[Option[Expression]]) = {
      val oldItem = myItem
      myItem = newItem
      emit(ChangeEvents.ItemChangedEvent(oldItem))
    }

    def exprItem = 
      item match {
        case Neutral(expr) => expr
        case _ => throw new IllegalArgumentException("Expression error")
      }

    override def toString = "ExprCell(" ++ item.toString ++ ")@" ++ hashCode.toString
  }

}
