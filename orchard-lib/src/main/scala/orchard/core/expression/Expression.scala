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

case class Variable(val ident : Identifier, val isThin : Boolean) extends Expression {

  def styleString = if (isThin) "var-thin" else "var"

}

case class Filler(val ident : Identifier, bdryIdent : Identifier, var bdryIsThin : Boolean) extends Expression { thisFiller =>

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

// object Boundary {

//   def unapply(expr : Expression) : Option[Filler] = {
//     if (expr.isInstanceOf[Filler.Boundary.type]) {
//       // Some(expr.asInstanceOf[Filler.Boundary].interior)
//       None
//     } else None
//   }
// }
