/**
  * Expression.scala - Expression definitions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

sealed trait Expression

case class Variable extends Expression

case class Filler extends Expression { thisFiller =>

  object Boundary extends Expression

}
