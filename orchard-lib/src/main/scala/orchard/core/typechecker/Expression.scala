/**
  * Expression.scala - Expression for Orchard
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.typechecker

import orchard.core.cell._

sealed trait Expression
case class Variable(name : String, shell : NCell[Option[Expression]]) extends Expression
case class Filler(name : String, nook : NCell[Option[Expression]]) extends Expression
case class Reference(name : String) extends Expression


