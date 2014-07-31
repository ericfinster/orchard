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

case class Variable(val ident : Identifier, val shell : Shell, val isThin : Boolean) extends Expression {

  def name = ident.expand
  def styleString = "variable"

}

case class Filler(val ident : Identifier, val nook : Nook) extends Expression {

  def name = "def-" ++ ident.expand
  def styleString = "filler"
  def isThin = true

  trait BoundaryExpression extends Expression {

    def name = ident.expand
    def styleString = "boundary"

    // Right, the idea is that this is part of the nook ....
    def isThin = nook.boundaryIsThin

  }

  object Boundary extends BoundaryExpression

}
