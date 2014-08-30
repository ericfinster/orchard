/**
  * Tree.scala - Higher Dimensional Trees
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.cell

import scala.language.higherKinds
import scala.language.implicitConversions

import scalaz._
import scalaz.Id._
import scalaz.Leibniz._

import Slice._

sealed abstract class TreeType[T[+_]](implicit idSlice : SliceOf[Id, T]) {

  type Tree[+A]

  def treeCoh[A] : Tree[A] === T[A]
  def treeCoe[A] : T[A] === Tree[A]

  type Dir
  type Address = List[Dir]

  type Context[+_] 
  type Derivative[+_]
  type Zipper[+A] = (Tree[A] , Context[A])

  def map[A, B](tree : Tree[A])(f : A => B) : Tree[B]
  def traverse[G[_], A, B](tree : Tree[A])(f : A => G[B])(implicit apG : Applicative[G]) : G[Tree[B]]
  def sequence[G[_], A](tree : Tree[G[A]])(implicit apG : Applicative[G]) : G[Tree[A]] =
    traverse(tree)(identity)

  def const[A, B](tree : Tree[A], b : B) : Tree[B] =
    map(tree)(_ => b)

  def plug[A](d : Derivative[A], a : A) : Tree[A]
  def close[A](c : Context[A], ta : Tree[A]) : Tree[A]

  def visit[A](dir : Dir, z : Zipper[A]) : Option[Zipper[A]]
  def seek[A](addr : Address, z : Zipper[A]) : Option[Zipper[A]] = 
    addr match {
      case Nil => Some(z)
      case d :: ds => 
        for {
          c <- seek(ds, z)
          v <- visit(d, c)
        } yield v
    }

  def nGlob[A](a : A) : Tree[A]
  def globDeriv : Derivative[List[Address]]

  def zipComplete[A, B](ta : Tree[A], tb : Tree[B]) : Option[Tree[(A, B)]]
  def zipWithCorolla[A](tree : Tree[A]) : Tree[(A, Derivative[A])]
  def zipWithPrefix[A](pref : Address, tree : Tree[A]) : Tree[(A, Address)] 
  def zipWithAddress[A](tree : Tree[A]) : Tree[(A, Address)] =
    zipWithPrefix(Nil, tree)

  def flattenWithPrefix[A](
    pref : List[Address], 
    deriv : Derivative[List[Address]], 
    tree : Slice[Tree, A]) : Option[Tree[(A, List[Address])]] = ???

  def flattenWithAddress[A](tree : Slice[Tree, A]) : Option[Tree[(A, List[Address])]] = 
    flattenWithPrefix(Nil, globDeriv, tree)

  def substitute[A](tta : Tree[Tree[A]]) : Option[Tree[A]]
  def graft[A](tree : Slice[T, A], brs : T[Slice[T, A]]) : Option[Slice[T, A]] = ???
  def graftAt[A](addr : List[Address], tree : Slice[T, A], branch : Slice[T, A]) : Option[Slice[T, A]] = {

    val succTree = SuccTree[({ type L[+A] = Slice[T, A] })#L]

    val test = succTree.seek(addr, ???)

    // type Zipper[+A] = (Tree[A] , Context[A])

    ???
  }


}

case class ZeroTree[T[+_]](implicit zeroSlice : ZeroSliceOf[Id, T]) extends TreeType[T] {

  import zeroSlice._

  type Tree[+A] = Id[A]

  def treeCoh[A] : Tree[A] === T[A] = coh[A]
  def treeCoe[A] : T[A] === Tree[A] = coe[A]

  type Dir = Nothing

  type Context[+A] = Unit
  type Derivative[+A] = Unit

  def map[A, B](a : Tree[A])(f : A => B) : Tree[B] = f(a)
  def traverse[G[_], A, B](tree : Tree[A])(f : A => G[B])(implicit apG : Applicative[G]) : G[Tree[B]] = f(tree)

  def plug[A](d : Derivative[A], a : A) : Tree[A] = a
  def close[A](c : Context[A], ta : Tree[A]) : Tree[A] = ta
  def visit[A](dir : Dir, z : Zipper[A]) : Option[Zipper[A]] = Some(z)

  def nGlob[A](a : A) : Tree[A] = a
  def globDeriv : Derivative[List[Address]] = ()

  def zipComplete[A, B](ta : Tree[A], tb : Tree[B]) : Option[Tree[(A, B)]] = Some(ta, tb)
  def zipWithCorolla[A](tree : Tree[A]) : Tree[(A, Derivative[A])] = (tree, ())
  def zipWithPrefix[A](pref : Address, tree : Tree[A]) : Tree[(A, Address)] = (tree, Nil)

  def substitute[A](a : A) : Option[A] = Some(a)

}

case class SuccTree[T[+_]](implicit succSlice : SuccSliceOf[Id, T]) extends TreeType[T] {

  import TreeType._
  import succSlice._

  // This is a bit annoying.  Maybe this should be the parameter?
  implicit val prevTree : TreeType[P] = implicitly[TreeType[P]]

  type Out[+A] = T[A]
  type Tree[+A] = Slice[P, A]

  def treeCoh[A] : Tree[A] === T[A] = coh[A]
  def treeCoe[A] : T[A] === Tree[A] = coe[A]

  implicit def prevCoh[A](s : prevTree.Tree[A]) : P[A] = 
    subst(s)(prevTree.treeCoh[A])

  implicit def prevCoe[A](p : P[A]) : prevTree.Tree[A] =
    subst(p)(prevTree.treeCoe[A])

  // Okay.  The major problem here is that these types don't compute.  I was managing it in
  // the reverse direction, but in the forward direction, it's just a nightmare.  I think what
  // you need to do is have a type class which witnesses that these guys are exactly one slice
  // apart, either forward or backward.

  // When these types are tied to an instance, it stops all computation at the type level.
  // So you'll have to take another go at it tomorrow....

  type Dir = List[prevTree.Dir]

  type Context[+A] = List[(A, prevTree.Derivative[Tree[A]])]
  type Derivative[+A] = (P[Tree[A]], Context[A])

  trait Unfolding[STP <: SuccTree[P]] {

    val st : STP

    type PP[+A] = st.succSlice.P[A]
    type PT[+A] = st.Tree[A]
    type PC[+A] = st.Context[A]

    type UnfoldedContext[+A] = List[(A, (PP[PT[Tree[A]]], PC[Tree[A]]))]
    type UnfoldedDerivative[+A] = (P[Tree[A]], UnfoldedContext[A])
    type UnfoldedZipper[+A] = (Tree[A], UnfoldedContext[A])

    type UnfoldedDir = List[List[st.prevTree.Dir]]

    def conCoh[A] : Context[A] === UnfoldedContext[A] = ???
    def derCoh[A] : Derivative[A] === UnfoldedDerivative[A] = ???

    def zipCoh[A] : Zipper[A] === UnfoldedZipper[A] = ???
    def zipCoe[A] : UnfoldedZipper[A] === Zipper[A] = ???

    def dirCoh : Dir === UnfoldedDir = ???

  }

  def unfoldWith(succP : SuccTree[P]) : Unfolding[succP.type] = 
    new Unfolding[succP.type] {
      val st : succP.type = succP
    }


  def map[A, B](tree : Tree[A])(f : A => B) : Tree[B] =
    tree match {
      case Cap() => Cap()
      case Joint(a, shell) => {
        Joint(f(a), prevTree.map(shell)(this.map(_)(f)))
      }
    }

  def traverse[G[_], A, B](tree : Tree[A])(f : A => G[B])(implicit apG : Applicative[G]) : G[Tree[B]] = ???

  def plug[B](d : Derivative[B], b : B) : Tree[B] =
    d match {
      case (shell, cntxt) => close(cntxt, Joint(b, shell))
    }

  def close[B](cntxt : Context[B], tb : Tree[B]) : Tree[B] = 
    cntxt match {
      case Nil => tb
      case (b, pd) :: cs =>
        close(cs, Joint(b, prevTree.plug(pd, tb)))
    }

  def visit[B](dir : Dir, z : Zipper[B]) : Option[Zipper[B]] =
    prevTree match {
      case zeroP : ZeroTree[P] => ???
      case succP : SuccTree[P] => {

        val unfolding = unfoldWith(succP)
        import unfolding._

        val uz : UnfoldedZipper[B] = subst(z)(zipCoh[B])
        val ud : UnfoldedDir = subst(dir)(dirCoh)

        uz match {
          case (Cap(), c) => None
          case (Joint(b, shell), c) => {

            val coeShell : st.Tree[Tree[B]] = subst(shell)(st.treeCoe[Tree[B]])

            st.seek(ud, (coeShell, Nil)) match {
              case None => None
              case Some((Cap(), z0)) => None
              case Some((Joint(t, tsh), z0)) => {
                Some(subst((t , (b , (tsh , z0)) :: c))(zipCoe[B]))
              }
            }
          }
        }
      }
    }

  def nGlob[A](a : A) : Tree[A] = Joint(a, prevTree.nGlob[Tree[A]](Cap()))
  def globDeriv : Derivative[List[Address]] = {
    val step1 : Tree[List[Address]] = nGlob(Nil)
    val step2 : P[Tree[List[Address]]] = prevTree.nGlob(step1)
    (step2, Nil)
  }

  def zipComplete[A, B](ta : Tree[A], tb : Tree[B]) : Option[Tree[(A, B)]] = 
    (ta, tb) match {
      case (Cap(), Cap()) => Some(Cap())
      case (Joint(a, ash), Joint(b, bsh)) => {

        import scalaz.syntax.monad._
        import scalaz.std.option._

        val step : Option[prevTree.Tree[(Tree[A], Tree[B])]] = prevTree.zipComplete(ash, bsh)
        val step2 : Option[P[(Tree[A], Tree[B])]] = prevTree.treeCoh[(Tree[A], Tree[B])].subst[Option](step)
        val step3 : Option[P[Option[Tree[(A, B)]]]] =
          for {
            s <- step2
          } yield prevTree.map(s)({ case (t0, t1) => zipComplete(t0, t1) })

        val step4 : Option[Option[prevTree.Tree[Tree[(A, B)]]]] = 
          for {
            s <- step3
          } yield prevTree.sequence[Option, Tree[(A, B)]](s)

        val step5 : Option[prevTree.Tree[Tree[(A, B)]]] = step4.join
        val step6 : Option[P[Tree[(A, B)]]] = prevTree.treeCoh[Tree[(A, B)]].subst[Option](step5)

        val step7 : Option[Tree[(A, B)]] = 
          for {
            s <- step6
          } yield Joint((a, b), s)

        step7
      }
      case _ => None
    }

  def zipWithCorolla[A](tree : Tree[A]) : Tree[(A, Derivative[A])] = 
    tree match {
      case Cap() => Cap()
      case Joint(a, shell) => {

        val step1 : P[Tree[(A, Derivative[A])]] = prevTree.map(shell)(t => zipWithCorolla(t))
        val step2 : P[Tree[A]] = prevTree.const(shell, Cap[P, A]())
        val step3 : Tree[(A, Derivative[A])] = Joint((a, (step2 , Nil)), step1)

        step3
      }
    }

  def zipWithPrefix[A](pref : Address, tree : Tree[A]) : Tree[(A, Address)] = 
    tree match {
      case Cap() => Cap()
      case Joint(a, shell) => {

        val step1 : P[(Tree[A], Dir)] = prevTree.zipWithAddress(shell)
        val step2 : P[Tree[(A, Address)]] = prevTree.map(step1)({ case (t, d) => zipWithPrefix(d :: pref, t) })
        val step3 : Tree[(A, Address)] = Joint((a, pref), step2)

        step3

      }
    }

  def substitute[A](tta : Tree[Tree[A]]) : Option[Tree[A]] = 
    tta match {
      case Cap() => Some(Cap())
      case Joint(t, tsh) => {

        import TreeType._
        import scalaz.std.option._

        val step1 : P[Option[Tree[A]]] = prevTree.map(tsh)(substitute)
        val step2 : Option[prevTree.Tree[Tree[A]]] = prevTree.sequence[Option, Tree[A]](step1)
        val step3 : Option[P[Tree[A]]] = prevTree.treeCoh[Tree[A]].subst(step2)
        val step4 : Option[Tree[A]] =
          for {
            s <- step3
            g <- prevTree.graft(t, s)
          } yield g

        step4
      }
    }

}

object TreeType {

  type Tree0[+A] = Id[A]
  type Tree1[+A] = Slice[Tree0, A]
  type Tree2[+A] = Slice[Tree1, A]
  type Tree3[+A] = Slice[Tree2, A]

  implicit def sliceOfIdIsTree[T[+_]](implicit sliceOf : SliceOf[Id, T]) : TreeType[T] =
    sliceOf match {
      case z : ZeroSliceOf[Id, T] => ZeroTree[T]()(z)
      case s : SuccSliceOf[Id, T] => SuccTree[T]()(s)
    }

  implicit def treeIsTraverse[T[+_]](implicit tt : TreeType[T]) : Traverse[T] = 
    new Traverse[T] {

      def traverseImpl[G[_], A, B](tree : T[A])(f : A => G[B])(implicit apG : Applicative[G]) : G[T[B]] = {
        val coeTree : tt.Tree[A] = subst(tree)(tt.treeCoe[A])
        val trTree : G[tt.Tree[B]] = tt.traverse(coeTree)(f)
        val cohTr : G[T[B]] = tt.treeCoh[B].subst[G](trTree)
        cohTr
      }

    }

  implicitly[TreeType[Tree0]]
  implicitly[TreeType[Tree1]]
  implicitly[TreeType[Tree2]]

  implicitly[Traverse[Tree3]]
  // implicitly[Functor[Tree2]]

}
