/**
  * Expression.scala - Opetopic Expressions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.checker

sealed trait Expression {

  def name : String
  def styleString : String
  def isThin : Boolean

}
