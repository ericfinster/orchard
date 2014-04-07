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

  def map(f : NCell[Expression] => NCell[Expression]) : EnvironmentNode = 
    this match {
      case g @ GroupNode(name) => {
        val node = GroupNode(name)
        node.children ++= g.children map (n => n map f)
        node
      }
      case e @ ExpressionNode(expr) => ExpressionNode(f(expr))
    }

  override def clone : EnvironmentNode = 
    this match {
      case g @ GroupNode(name) => {
        println("Cloning group " ++ name ++ " with " ++ g.children.length.toString ++ " children.")
        val node = GroupNode(name)
        val chldrn = g.children map (_.clone)
        node.children ++= chldrn
        node
      }
      case ExpressionNode(expr) => {
        println("Cloning node for: " ++ expr.value.toString)
        ExpressionNode(expr)
      }
    }
}

case class GroupNode(val name : String) extends EnvironmentNode {

  def flatten : Seq[NCell[Expression]] = children flatMap (_.flatten)
  val children : Buffer[EnvironmentNode] = Buffer.empty

}

case class ExpressionNode(val expr : NCell[Expression]) extends EnvironmentNode {

  def flatten : Seq[NCell[Expression]] = Seq(expr)

}


