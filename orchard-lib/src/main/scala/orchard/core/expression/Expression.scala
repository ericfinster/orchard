/**
  * Expression.scala - Expression definitions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

sealed trait Expression {

  def ident : Identifier
  def isThin : Boolean
  def styleString : String

  override def toString = ident.toString

}

case class Variable(val ident : Identifier, val isThin : Boolean) extends Expression {

  def styleString = if (isThin) "var-thin" else "var"

}

case class Filler(val ident : Identifier, bdryIdent : Identifier, bdryIsThin : Boolean) extends Expression { thisFiller =>

  def isThin : Boolean = true
  def styleString = "filler"

  object Boundary extends Expression {

    def ident = bdryIdent
    def interior = thisFiller
    def isThin = bdryIsThin
    def styleString = if (isThin) "filler-tgt-thin" else "filler-tgt"

  }

}
