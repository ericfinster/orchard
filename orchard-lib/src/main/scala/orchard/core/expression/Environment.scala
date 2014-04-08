/**
  * Environment.scala - Routines for cells in an environment
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression


import scala.collection.mutable.Buffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

import orchard.core.cell._

sealed trait EnvironmentNode {

  def toSeq : Seq[NCell[Expression]]
  def toExprSeq : Seq[Expression]

  def lookup(id : String) : Option[NCell[Expression]] = 
    toSeq find (expr => expr.value.toString == id)

  def contains(id : String) : Boolean = 
    toSeq exists (expr => expr.value.toString == id)

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

  def toSeq : Seq[NCell[Expression]] = children flatMap (_.toSeq)
  def toExprSeq : Seq[Expression] = children flatMap (_.toExprSeq)

  val children : Buffer[EnvironmentNode] = Buffer.empty

}

case class ExpressionNode(val expr : NCell[Expression]) extends EnvironmentNode {

  def toSeq : Seq[NCell[Expression]] = Seq(expr)
  def toExprSeq : Seq[Expression] = Seq(expr.value)

}

object EnvironmentNode {

  def clone(node : EnvironmentNode) : EnvironmentNode = {
    val exprMap = HashMap.empty[Expression, Expression]
    val waitset = HashSet.empty[Expression]

    def identIsComplete(ident : Identifier) : Boolean = 
      ident.exprRefs forall (expr => exprMap.isDefinedAt(expr))

    def cloneIdent(ident : Identifier) : Identifier = 
      Identifier(
        ident.tokens map {
          case LiteralToken(lit) => LiteralToken(lit)
          case ExpressionToken(expr) => ExpressionToken(exprMap(expr))
        }
      )

    def exprIsComplete(expr : Expression) : Boolean = 
      expr match {
        case Variable(ident, isThin) => identIsComplete(ident)
        case Filler(ident, bdryIdent, bdryIsThin) => 
          identIsComplete(ident) && identIsComplete(bdryIdent)
        case bdry : Filler#Boundary => exprMap.isDefinedAt(bdry.interior)
      }

    def cloneExpr(expr : Expression) : Expression = {
      expr match {
        case Variable(ident, isThin) => Variable(cloneIdent(ident), isThin)
        case Filler(ident, bdryIdent, bdryIsThin) => 
          Filler(cloneIdent(ident), cloneIdent(bdryIdent), bdryIsThin)
        case bdry : Filler#Boundary =>
          exprMap(bdry.interior).asInstanceOf[Filler].MyBoundary
      }
    }

    // Do the initial pass
    node.toExprSeq foreach (expr => {
      if (exprIsComplete(expr)) {
        exprMap(expr) = cloneExpr(expr)
      } else {
        waitset += expr
      }
    })

    while (waitset.size > 0) {
      var progressCount : Int = 0

      waitset foreach (expr => {
        if (exprIsComplete(expr)) {
          exprMap(expr) = cloneExpr(expr)
          waitset -= expr
          progressCount += 1
        }
      })

      if (progressCount == 0) {
        throw new IllegalStateException("Failed to make progress in translation.")
      }
    } 

    node map (ncell => {
      ncell map (expr => exprMap(expr))
    })
  }
}

