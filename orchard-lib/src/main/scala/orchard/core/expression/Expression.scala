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

  // Use very strict comparison
  override def equals(other : Any) : Boolean = 
    super.equals(other)

  override def hashCode = super.hashCode
  override def toString = ident.toString

}

case class Variable(var ident : Identifier, var isThin : Boolean) extends Expression {

  def styleString = if (isThin) "var-thin" else "var"

}

case class Filler(var ident : Identifier, var bdryIdent : Identifier, var bdryIsThin : Boolean) extends Expression { thisFiller =>

  def isThin : Boolean = true
  def styleString = "filler"

  trait Boundary extends Expression {

    def ident = bdryIdent
    def interior = thisFiller
    def isThin = bdryIsThin  // This can be changed by substitutions
    def styleString = if (isThin) "bdry-thin" else "bdry"

  }

  case object MyBoundary extends Boundary

}

sealed trait RawExpression
case class RawVariable(val ident : RawIdentifier, val isThin : Boolean) extends RawExpression
case class RawFiller(val ident : RawIdentifier, val bdryIdent : RawIdentifier, val bdyIsThin : Boolean) extends RawExpression
