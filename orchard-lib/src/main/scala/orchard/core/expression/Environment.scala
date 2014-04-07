/**
  * Environment.scala - Routines for cells in an environment
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

import scala.collection.mutable.Buffer

import orchard.core.cell._

sealed trait EnvironmentNode {

  def flatten : Seq[NCell[Expression]]

  def lookup(id : String) : Option[NCell[Expression]] = 
    flatten find (expr => expr.value.toString == id)

  def contains(id : String) : Boolean = 
    flatten exists (expr => expr.value.toString == id)

}

case class GroupNode(val name : String) extends EnvironmentNode {

  def flatten : Seq[NCell[Expression]] = children flatMap (_.flatten)
  val children : Buffer[EnvironmentNode] = Buffer.empty

}

case class ExpressionNode(val expr : NCell[Expression]) extends EnvironmentNode {

  def flatten : Seq[NCell[Expression]] = Seq(expr)

}


