/**
  * RoseTree.scala - Rose Trees
  * 
  * @author Eric Finster
  * @version 0.1
  */

package orchard.core.util

import scala.collection.mutable.ListBuffer

import ErrorM._

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

    def mapCells[S](f : A => S) : RoseTree[S, B] = 
      map(f, (r => r))

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

    def forallCells(p : A => Boolean) : Boolean = 
      tree match {
        case Rose(_) => true
        case Branch(value, branches) => {
          if (p(value)) { branches forall (_.forallCells(p)) } else false
        }
      }

    def leaves : Vector[B] =
      tree match {
        case Rose(value) => Vector[B](value)
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

    def rootElement : Error[A] =
      tree match {
        case Rose(_) => fail("Root element called on a rose.")
        case Branch(value, _) => succeed(value)
      }


  }

  implicit def roseTreeIsReadable[A, B, P](
    implicit aReader : JsonReadable[A, P], bReader : JsonReadable[B, P]
  ) : JsonReadable[RoseTree[A, B], P] = new JsonReadable[RoseTree[A, B], P] {
    def read(x : P, reader : JsonReader[P]) : RoseTree[A, B] = {
      reader.readString(
        reader.readObjectField(x, "type")
      ) match {
        case "rose" => {
          val bObj = reader.readObjectField(x, "value")
          val b = bReader.read(bObj, reader)
          Rose(b)
        }
        case "branch" => {
          val vReader = implicitly[JsonReadable[Vector[RoseTree[A, B]], P]]

          val aObj = reader.readObjectField(x, "value")
          val a = aReader.read(aObj, reader)
          val branchesObj = reader.readObjectField(x, "branches")
          val branches = vReader.read(branchesObj, reader)

          Branch(a, branches)
        }
      }
    }
  }

  implicit def roseTreeIsWritable[A, B, P](
    implicit aWriter : JsonWritable[A, P], bWriter : JsonWritable[B, P]
  ) : JsonWritable[RoseTree[A, B], P] = new JsonWritable[RoseTree[A, B], P] {
    def write(tree : RoseTree[A, B], writer : JsonWriter[P]) : P = {
      tree match {
        case Rose(b) => {
          writer.writeObject(
            ("type" -> writer.writeString("rose")),
            ("value" -> bWriter.write(b, writer))
          )
        }
        case Branch(a, branches) => {
          writer.writeObject(
            ("type" -> writer.writeString("branch")),
            ("value" -> aWriter.write(a, writer)),
            ("branches" -> writer.writeArray((branches map (write(_, writer))) : _*))
          )
        }
      }
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

  def zipOnce : Error[RoseZipper[A, B]] =
    context match {
      case RoseContext(value, left, right) :: cs => 
        succeed(RoseZipper(Branch(value, left ++ List(focus) ++ right), cs))
      case _ => fail("Cannot unzip empty context.")
    }

  def toAddrBuffer : ListBuffer[Int] = 
    context match {
      case Nil => new ListBuffer[Int]
      case RoseContext(value, left, right) :: cs =>
        RoseZipper(Branch(value, left ++ Vector(focus) ++ right), cs).toAddrBuffer += left.length
    }

  def toAddr : List[Int] = toAddrBuffer.toList

  def leftSibling : Error[RoseZipper[A, B]] =
      context match {
        case RoseContext(value, left, right) :: cs => {
          for {
            leftSib <- left.lastOption
          } yield RoseZipper(leftSib, RoseContext(value, left.init, focus +: right) :: cs)
        }
        case _ => fail("No left sibling")
      }

  def rightSibling : Error[RoseZipper[A, B]] =
      context match {
        case RoseContext(value, left, right) :: cs => {
          for {
            rightSib <- right.headOption
          } yield RoseZipper(rightSib, RoseContext(value, left :+ focus, right.tail) :: cs)
        }
        case _ => fail("No right sibling")
      }

  def visitBranch(i : Int) : Error[RoseZipper[A, B]] =
      focus match {
        case Rose(_) => fail("No branch here.")
        case Branch(value, branches) => {
          if (i > branches.length - 1) None else {
            val (left, rightPlus) = branches.splitAt(i)
            succeed(RoseZipper(rightPlus.head, RoseContext(value, left, rightPlus.tail) :: context))
          }
        }
      }

  def seek(addr : List[Int]) : Error[RoseZipper[A, B]] =
      addr match {
        case Nil => succeed(this)
        case i :: is => 
          for {
            next <- visitBranch(i)
            res <- next.seek(is)
          } yield res
      }

  def find(branchProp : A => Boolean, roseProp : B => Boolean) : Error[RoseZipper[A, B]] = 
    focus match {
      case Rose(value) => if (roseProp(value)) succeed(this) else fail("Find failed.")
      case Branch(value, branches) => {
        if (branchProp(value)) succeed(this) else {
          // Ummm ... got a better way?
          var i : Int = 0

          while (i < branches.length) {
            val res = visitBranch(i).get.find(branchProp, roseProp)
            if (res.isSuccess) return res
            i += 1
          }

          fail("Find failed.")
        }
      }
    }

  def find(prop : A => Boolean) : Error[RoseZipper[A, B]] =
    find(prop, (_ => false))

  def lookup(v : A) : Error[RoseZipper[A, B]] =
    find((a => v == a))
}
