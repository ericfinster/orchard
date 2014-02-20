/**
  * ExpressionPanel.scala - A trait for panels displaying expressions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

trait ExpressionPanel extends RenderingPanel[Polarity[Option[Expression]]] {

  override type ComplexType <: ExpressionWorksheet

}
