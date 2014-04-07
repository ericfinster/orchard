/**
  * RoseTree.scala - Rose Trees
  * 
  * @author Eric Finster
  * @version 0.1
  */

package orchard.core.util

import scala.collection.mutable.ListBuffer

import Util._

sealed trait RoseTree[+A, +B]
case class Rose[B](value : B) extends RoseTree[Nothing, B] { override def toString = "(Leaf " ++ value.toString ++ ")" }
case class Branch[A, B](value : A, branches : Vector[RoseTree[A, B]]) extends RoseTree[A, B]
{ override def toString = "(Branch: " ++ value.toString ++ ") => " ++ (branches map (_.toString)).toString }

object RoseTree {
  implicit class RoseTreeOps[A, B](tree : RoseTree[A, B]) {
    def map[S, T](f : A => S, g : B => T) : RoseTree[S, T] = 
      tree match {
        case Rose(value) => Rose(g(value))
        case Branch(value, branches) => 
          Branch(f(value), branches map (b => b.map(f, g)))
      }

    def foreach(branchAction : A => Unit, 
                roseAction : B => Unit) : Unit =
      tree match {
        case Rose(value) => roseAction(value)
        case Branch(value, branches) => {
          branches foreach (branch => branch.foreach(branchAction, roseAction))
          branchAction(value)
        }
      }

    def foreachCell(action : A => Unit) : Unit =
      foreach(action, _ => ())

    def leaves : Vector[B] =
      tree match {
        case Rose(value) => Vector(value)
        case Branch(value, branches) => branches flatMap (_.leaves)
      }

    def toList : List[A] = 
      tree match {
        case Rose(_) => Nil
        case branch => {
          val buf = new ListBuffer[A]
          branch.foreach((value => buf += value), (_ => ()))
          buf.toList
        }
      }

    def nodeVector : Vector[A] = 
      tree match {
        case Rose(_) => Vector.empty
        case Branch(value, branches) => {
          (branches flatMap (_.nodeVector)) :+ value
        }
      }

    def isRose : Boolean =
      tree match {
        case Rose(_) => true
        case _ => false
      }

    def rootElement : Option[A] =
      tree match {
        case Rose(_) => None
        case Branch(value, _) => Some(value)
      }
  }
}

case class RoseContext[A, B](value : A, 
                             left : Vector[RoseTree[A, B]],
                             right : Vector[RoseTree[A, B]])

case class RoseZipper[A, B](val focus : RoseTree[A, B], 
                            val context : List[RoseContext[A, B]]) {

  def setFocus(tree : RoseTree[A, B]) : RoseZipper[A, B] =
    RoseZipper(tree, context)

  def zip : RoseTree[A, B] =
      context match {
        case Nil => focus
        case RoseContext(value, left, right) :: cs => 
          RoseZipper(Branch(value, left ++ List(focus) ++ right), cs).zip
      }

  def zipOnce : Option[RoseZipper[A, B]] =
    context match {
      case RoseContext(value, left, right) :: cs => 
        Some(RoseZipper(Branch(value, left ++ List(focus) ++ right), cs))
      case _ => None
    }

  def toAddrBuffer : ListBuffer[Int] = 
    context match {
      case Nil => new ListBuffer[Int]
      case RoseContext(value, left, right) :: cs =>
        RoseZipper(Branch(value, left ++ Vector(focus) ++ right), cs).toAddrBuffer += left.length
    }

  def toAddr : List[Int] = toAddrBuffer.toList

  def leftSibling : Option[RoseZipper[A, B]] =
      context match {
        case RoseContext(value, left, right) :: cs => {
          for {
            leftSib <- left.lastOption
          } yield RoseZipper(leftSib, RoseContext(value, left.init, focus +: right) :: cs)
        }
        case _ => None
      }

  def rightSibling : Option[RoseZipper[A, B]] =
      context match {
        case RoseContext(value, left, right) :: cs => {
          for {
            rightSib <- right.headOption
          } yield RoseZipper(rightSib, RoseContext(value, left :+ focus, right.tail) :: cs)
        }
        case _ => None
      }

  def visitBranch(i : Int) : Option[RoseZipper[A, B]] =
      focus match {
        case Rose(_) => None
        case Branch(value, branches) => {
          if (i > branches.length - 1) None else {
            val (left, rightPlus) = branches.splitAt(i)
            Some(RoseZipper(rightPlus.head, RoseContext(value, left, rightPlus.tail) :: context))
          }
        }
      }

  def seek(addr : List[Int]) : Option[RoseZipper[A, B]] =
      addr match {
        case Nil => Some(this)
        case i :: is => 
          for {
            next <- visitBranch(i)
            res <- next.seek(is)
          } yield res
      }

  def find(prop : A => Boolean) : Option[RoseZipper[A, B]] =
    focus match {
      case Rose(_) => None
      case Branch(value, branches) => {
        if (prop(value)) Some(this) else {
          // Ummm ... got a better way?
          var i : Int = 0

          while (i < branches.length) {
            val res = visitBranch(i).force.find(prop)
            if (res != None) return res
            i += 1
          }

          None
        }
      }
    }

  def lookup(v : A) : Option[RoseZipper[A, B]] =
    find((a => v == a))
}
