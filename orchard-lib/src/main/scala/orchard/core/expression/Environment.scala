/**
  * Environment.scala - Routines for cells in an environment
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

import scala.collection.mutable.Map
import scala.collection.mutable.Buffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

import orchard.core.cell._
import orchard.core.util._

sealed trait EnvironmentElement
case class GroupElement(val name : String) extends EnvironmentElement
case class ExpressionElement(val ncell : NCell[Expression]) extends EnvironmentElement

trait EnvironmentType[A] {

  type ChildCollectionType <: Buffer[A]

  def children(a : A) : ChildCollectionType

  def getElement(a : A) : EnvironmentElement
  def setElement(a : A, el : EnvironmentElement) : Unit

  def getParent(a : A) : Option[A]
  def setParent(a : A, parent : A) : Unit

  def createNode(el : EnvironmentElement) : A

  def appendElement(a : A, el : EnvironmentElement) : Unit = {
    children(a) += createNode(el)
  }

  def appendNode(a : A, child : A) : Unit = {
    children(a) += child
  }

  def removeElement(a : A, child : A) : Unit = {
    children(a) -= child
  }

  def delete(a : A) : Unit = 
    for {
      parent <- getParent(a)
    } { 
      removeElement(parent, a) 

      if (children(parent).isEmpty) 
        delete(parent)
    }

  def toSeq(a : A) : Seq[A] = 
    getElement(a) match {
      case GroupElement(_) => children(a) flatMap (toSeq(_))
      case ExpressionElement(_) => Seq(a)
    }

  def toExprSeq(a : A) : Seq[Expression] =
    getElement(a) match {
      case GroupElement(_) => children(a) flatMap (toExprSeq(_))
      case ExpressionElement(ncell) => Seq(ncell.value)
    }

  def toNCellSeq(a : A) : Seq[NCell[Expression]] = 
    getElement(a) match {
      case GroupElement(_) => children(a) flatMap (toNCellSeq(_))
      case ExpressionElement(ncell) => Seq(ncell)
    }

  def foreachNCell(a : A, op : NCell[Expression] => Unit) : Unit = 
    foreach(a, (child => 
      getElement(child) match {
        case ExpressionElement(ncell) => op(ncell)
        case _ => ()
      })
    )

  def foreachExpr(a : A, op : Expression => Unit) : Unit =
    foreachNCell(a, (ncell => op(ncell.value)))

  def foreach(a : A, op : A => Unit) : Unit = {
    children(a) foreach (child => foreach(child, op))
    op(a)
  }

  def findByExpression(a : A, expr : Expression) : Option[A] =
    toSeq(a) find (getExpression(_) == expr)

  def findById(a : A, id : String) : Option[A] =
    toSeq(a) find (getExpression(_).id == id)

  def getNCell(a : A) : NCell[Expression] =
    getElement(a) match {
      case GroupElement(_) => throw new NoSuchElementException
      case ExpressionElement(ncell) => ncell
    }

  def getExpression(a : A) : Expression = 
    getNCell(a).value

  def mapExprs(a : A, f : NCell[Expression] => NCell[Expression]) : Unit = 
    foreach(a, (child =>
      getElement(child) match {
        case ExpressionElement(ncell) => setElement(child, ExpressionElement(f(ncell)))
        case _ => ()
      })
    )
    
  def cloneFrom[B](b : B, bOps : EnvironmentType[B]) : A = {
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
    bOps.foreachExpr(b, (expr => {
      if (exprIsComplete(expr)) {
        exprMap(expr) = cloneExpr(expr)
      } else {
        waitset += expr
      }
    }))

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

    def replicate(b : B) : A = 
      bOps.getElement(b) match {
        case GroupElement(name) => {
          val node = createNode(GroupElement(name))

          children(node) ++= bOps.children(b) map (child => {
            val newChild = replicate(child)
            setParent(newChild, node)
            newChild
          })

          node
        }
        case ExpressionElement(ncell) => 
          createNode(ExpressionElement(ncell map (exprMap(_))))
      }

    replicate(b)
  }

  def toXML(a : A) : xml.Node = 
    getElement(a) match {
      case GroupElement(name) => <group name={name}>{children(a) map (toXML(_))}</group>
      case ExpressionElement(expr) => 
        <expression>{
          XmlSerializable.cellSerializable[String].toXML(expr map (_.hashCode.toString))
        }</expression>
    }
}

class SimpleEnvironmentNode(el : EnvironmentElement) {

  var element = el
  var parent : Option[SimpleEnvironmentNode] = None

  val children = Buffer.empty[SimpleEnvironmentNode]

}

object SimpleNodeImplementation extends EnvironmentType[SimpleEnvironmentNode] {

  type ChildCollectionType = Buffer[SimpleEnvironmentNode]

  def children(node : SimpleEnvironmentNode) = node.children

  def getElement(node : SimpleEnvironmentNode) = node.element
  def setElement(node : SimpleEnvironmentNode, el : EnvironmentElement) = node.element = el

  def getParent(node : SimpleEnvironmentNode) = node.parent
  def setParent(node : SimpleEnvironmentNode, parent : SimpleEnvironmentNode) = node.parent = Some(parent)

  def createNode(el : EnvironmentElement) : SimpleEnvironmentNode = new SimpleEnvironmentNode(el)

}

trait HasEnvironment {

  type EnvironmentNodeType

  def envOps : EnvironmentType[EnvironmentNodeType]
  def envRoot : EnvironmentNodeType

}

// sealed trait EnvironmentNode {

//   def toSeq : Seq[NCell[Expression]]
//   def toExprSeq : Seq[Expression]
//   def toNodeSeq : Seq[ExpressionNode]

//   def locateNode(expr : Expression) : Option[ExpressionNode] = {
//     toNodeSeq find (nd => nd.expr.value == expr)
//   }

//   def lookup(id : String) : Option[NCell[Expression]] = 
//     toSeq find (expr => expr.value.toString == id)

//   def contains(id : String) : Boolean = 
//     toSeq exists (expr => expr.value.toString == id)

//   def map(f : NCell[Expression] => NCell[Expression]) : Unit = 
//     foreach {
//       case e @ ExpressionNode(_) => { e.expr = f(e.expr) }
//       case _ => ()
//     }

//   def foreach(op : EnvironmentNode => Unit) : Unit = 
//     this match {
//       case g @ GroupNode(_) => { g.children foreach (n => n foreach(op)) ; op(g) }
//       case e @ ExpressionNode(_) => { op(e) }
//     }

//   var parent : Option[GroupNode] = None

//   private var deleteCallback : (() => Unit) = () => ()

//   def onDelete_=(op : =>Unit) = deleteCallback = (() => op)
//   def onDelete = deleteCallback

//   def delete : Unit = {
//     println("In delete method for " ++ this.toString)
//     parent foreach (p => {
//       p.children -= this
//       if (p.children.isEmpty) { println("Empty parent.") ; p.delete }
//     })

//     deleteCallback()
//   }
// }

// case class GroupNode(val name : String) extends EnvironmentNode {

//   def toSeq : Seq[NCell[Expression]] = children flatMap (_.toSeq)
//   def toExprSeq : Seq[Expression] = children flatMap (_.toExprSeq)
//   def toNodeSeq : Seq[ExpressionNode] = children flatMap(_.toNodeSeq)

//   val children : Buffer[EnvironmentNode] = Buffer.empty

// }

// case class ExpressionNode(e : NCell[Expression]) extends EnvironmentNode {

//   private var myExpr = e
//   private var updateCallback : (() => Unit) = () => ()

//   def onUpdate_=(op : =>Unit) = updateCallback = (() => op)
//   def onUpdate = updateCallback
  
//   def expr = myExpr
//   def expr_=(e : NCell[Expression]) = {
//     myExpr = e
//     updateCallback()
//   }

//   def toSeq : Seq[NCell[Expression]] = Seq(expr)
//   def toExprSeq : Seq[Expression] = Seq(expr.value)
//   def toNodeSeq : Seq[ExpressionNode] = Seq(this)

// }

// object EnvironmentNode {

//   def clone(node : EnvironmentNode) : EnvironmentNode = {
//     val exprMap = HashMap.empty[Expression, Expression]
//     val waitset = HashSet.empty[Expression]

//     def identIsComplete(ident : Identifier) : Boolean = 
//       ident.exprRefs forall (expr => exprMap.isDefinedAt(expr))

//     def cloneIdent(ident : Identifier) : Identifier = 
//       Identifier(
//         ident.tokens map {
//           case LiteralToken(lit) => LiteralToken(lit)
//           case ExpressionToken(expr) => ExpressionToken(exprMap(expr))
//         }
//       )

//     def exprIsComplete(expr : Expression) : Boolean = 
//       expr match {
//         case Variable(ident, isThin) => identIsComplete(ident)
//         case Filler(ident, bdryIdent, bdryIsThin) => 
//           identIsComplete(ident) && identIsComplete(bdryIdent)
//         case bdry : Filler#Boundary => exprMap.isDefinedAt(bdry.interior)
//       }

//     def cloneExpr(expr : Expression) : Expression = {
//       expr match {
//         case Variable(ident, isThin) => Variable(cloneIdent(ident), isThin)
//         case Filler(ident, bdryIdent, bdryIsThin) => 
//           Filler(cloneIdent(ident), cloneIdent(bdryIdent), bdryIsThin)
//         case bdry : Filler#Boundary =>
//           exprMap(bdry.interior).asInstanceOf[Filler].MyBoundary
//       }
//     }

//     // Do the initial pass
//     node.toExprSeq foreach (expr => {
//       if (exprIsComplete(expr)) {
//         exprMap(expr) = cloneExpr(expr)
//       } else {
//         waitset += expr
//       }
//     })

//     while (waitset.size > 0) {
//       var progressCount : Int = 0

//       waitset foreach (expr => {
//         if (exprIsComplete(expr)) {
//           exprMap(expr) = cloneExpr(expr)
//           waitset -= expr
//           progressCount += 1
//         }
//       })

//       if (progressCount == 0) {
//         throw new IllegalStateException("Failed to make progress in translation.")
//       }
//     } 

//     def replicate(nd : EnvironmentNode) : EnvironmentNode =
//       nd match {
//         case g @ GroupNode(name) => {
//           val newNode = GroupNode(name)
//           newNode.children ++= g.children map (child => {
//             val childNode = replicate(child)
//             childNode.parent = Some(newNode)
//             childNode
//           })

//           newNode
//         }
//         case e @ ExpressionNode(_) => ExpressionNode(e.expr map (exprMap(_)))
//       }

//     replicate(node)
//   }
// }



