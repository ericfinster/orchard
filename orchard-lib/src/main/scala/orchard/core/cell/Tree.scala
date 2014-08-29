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
import scalaz.syntax.monad._

import orchard.core.util._
import ErrorM._
import Nats._

sealed trait TreeType[T[+_]] {

  type Tree[+A] = T[A]

  type Pred[+A]
  val predTreeType : Option[TreeType[Pred]]

  type Dir
  type Address = List[Dir]

  type Context[+_] 
  type Derivative[+_]

  type Zipper[+A] = (Tree[A], Context[A])

  def plug[B](d : Derivative[B], b : B) : T[B]
  def close[B](c : Context[B], tb : T[B]) : T[B]

  def visit[B](dir : Dir, z : Zipper[B]) : Option[Zipper[B]]

  def seek[B](addr : Address, z : Zipper[B]) : Option[Zipper[B]] = 
    addr match {
      case Nil => Some(z)
      case d :: ds => 
        for {
          c <- seek(ds, z)
          v <- visit(d, c)
        } yield v
    }

}

case class ZeroTree() extends TreeType[Id] {

  type Pred[+A] = Nothing
  val predTreeType = None

  type Dir = Nothing

  type Context[+A] = Unit
  type Derivative[+A] = Unit

  def plug[B](d : Derivative[B], b : B) : Tree[B] = b
  def close[B](c : Context[B], tb : Tree[B]) : Tree[B] = tb

  def visit[B](dir : Dir, z : Zipper[B]) : Option[Zipper[B]] = None

}

case class SuccTree[P[+_]](val pt : TreeType[P]) extends TreeType[({ type L[+A] = Slice[P, A] })#L] {

  type Pred[+A] = P[A]
  val predTreeType = Some(pt)

  type Dir = List[pt.Dir]

  type Context[+A] = List[(A, pt.Derivative[Tree[A]])]
  type Derivative[+A] = (pt.Tree[Tree[A]], Context[A])

  def plug[B](d : Derivative[B], b : B) : Tree[B] =
    d match {
      case (shell, c) => close(c, Joint(b, shell))
    }

  def close[B](c : Context[B], tb : Tree[B]) : Tree[B] =
    c match {
      case Nil => tb
      case (b, pd) :: c => close(c, Joint(b, pt.plug(pd, tb)))
    }

  def visit[B](dir : Dir, z : Zipper[B]) : Option[Zipper[B]] = 
    pt.predTreeType match {
      case None => ???
      case Some(ppt) => {
        (dir, z) match {
          case (_ , (Cap() , _)) => None
          case (addr, (Joint(b, shell), _)) => {

            val test0 : pt.Tree[Tree[B]] = shell
            val test1 : pt.Zipper[Tree[B]] = (shell, Nil)
            // val test = pt.seek(addr, (shell, Nil))

            ???
          }
        }
      }
    }

}


object IsZeroTree {

  def unapply[P[+_]](tt : TreeType[P]) : Option[TreeType[Id]] =
    if (tt.isInstanceOf[ZeroTree]) {
      Some(tt.asInstanceOf[TreeType[Id]])
    } else None    

}

object IsSuccTree {

  def unapply[P[+_]](tt : TreeType[P]) : Option[TreeType[tt.Pred]] = 
    tt.predTreeType

}

object TreeType {


  type Tree0[+A] = Id[A]
  type Tree1[+A] = Slice[Tree0, A]
  type Tree2[+A] = Slice[Tree1, A]
  type Tree3[+A] = Slice[Tree2, A]


  implicit def idIsTreeType : TreeType[Id] = ZeroTree()

  implicit def sliceIsTreeType[P[+_]](implicit pt : TreeType[P]) 
      : TreeType[({ type L[+A] = Slice[P, A] })#L] = SuccTree(pt)

  implicitly[TreeType[Tree0]]
  implicitly[TreeType[Tree1]]
  implicitly[TreeType[Tree2]]

  implicit class TreeOps[T[+_], A](tr : T[A])(implicit isTree : TreeType[T]) {

  }

}

// sealed trait Tree {

//   type C[_]

//   type Dim <: Nat

//   type Addr

//   type Derivative[_]
//   type Context[_]

// }

// case class ZT() extends Tree {

//   type C[A] = Id[A]

//   type Dim = _0

//   type Addr = Nothing

//   type Derivative[A] = Unit
//   type Context[A] = Unit

// }

// case class ST[T <: Tree]() extends Tree {

//   type C[A] = Slice[T#C, A]

//   type Dim = S[T#Dim]

//   type Addr = List[T#Addr]

//   type Derivative[A] = (T#C[C[A]], Context[A])
//   type Context[A] = List[(A, T#Derivative[C[A]])]

// }

// object TreeTest {

//   type Tree0[A] = ZT#C[A]
//   type Tree1[A] = ST[ZT]#C[A]

//   implicit class TreeOps[T <: Tree, A](t : T#C[A]) {

//   }

// }

// trait SliceOf[F[_], G[_]] {

//   type P[_] 

//   trait PointMatch {
//     def unapply[A](t : G[A]) : Option[F[A]]
//   }

//   trait LeafMatch {
//     def unapply[A](t : G[A]) : Option[Unit]
//   }

//   trait BranchMatch {
//     def unapply[A](t : G[A]) : Option[(A , P[G[A]])]
//   }

//   val IsPoint : PointMatch
//   val IsLeaf : LeafMatch
//   val IsBranch : BranchMatch

// }

// trait IdSlice[G[_]] {

// }

// object SliceOf {

//   implicit def fSliceOfF[F[_]] : SliceOf[F, F] = ??? //new SliceOf[F, F] { }

//   implicit def sliceSliceOfF[F[_], G[_]](implicit sl : SliceOf[F, G]) : SliceOf[F, ({ type L[X] = Slice[G, X] })#L] = ???

//   type Tree0[+A] = Id[A]
//   type Tree1[+A] = Slice[Tree0, A]
//   type Tree2[+A] = Slice[Tree1, A]
//   type Tree3[+A] = Slice[Tree2, A]

//   implicitly[SliceOf[Id, Tree0]]
//   implicitly[SliceOf[Id, Tree1]]
//   implicitly[SliceOf[Id, Tree2]]

//   implicit class TreeOps[G[_], A](t : G[A])(implicit isTree : SliceOf[Id, G]) {

//     import isTree._

//     def pointValue : Option[A] =
//       t match {
//         case IsPoint(ia) => Some(ia)
//         case IsLeaf => None
//         case IsBranch(a, x) => {
//           val test : A = a
//           val test2 : P[G[A]] = x

//           None
//         }
//       }
//   }

// }

// trait IsTree[TA] {

//   type Dim <: Nat

//   type A

//   type T[+_]
//   type P[+_]

//   type Dir
//   type Addr = List[Dir]

//   type PrevDeriv[+_]
//   type Context[+X] = List[(X, PrevDeriv[T[X]])]
//   type Derivative[+X] = (P[T[X]], Context[X])

//   def plug(d : Derivative[A], a : A) : T[A]
//   def close(c : Context[A], t : T[A]) : T[A]

//   trait PointMatch {
//     def unapply(t : TA) : Option[A]
//   }

//   trait LeafMatch {
//     def unapply(t : TA) : Option[Unit]
//   }

//   trait BranchMatch {
//     def unapply(t : TA) : Option[(A , P[T[A]], IsTree[P[A]])]
//   }

//   val IsPoint : PointMatch
//   val IsLeaf : LeafMatch
//   val IsBranch : BranchMatch

// }


// sealed trait TreeAddr[D <: Nat]
// case class Root[D <: Nat]() extends TreeAddr[S[D]]
// case class Then[D <: Nat](dir : TreeAddr[D], addr : TreeAddr[S[D]]) extends TreeAddr[S[D]]

// sealed trait TreeDeriv[D <: Nat]
// case object PointDeriv extends TreeDeriv[_0]
// // case class Suspend[D <: Nat](shell : TTS

// object IsTree {

//   type Tree0[+A] = Id[A]
//   type Tree1[+A] = Slice[Tree0, A]
//   type Tree2[+A] = Slice[Tree1, A]
//   type Tree3[+A] = Slice[Tree2, A]

//   val isTree0 = implicitly[IsTree[Tree0[Int]]]
//   val isTree1 = implicitly[IsTree[Tree1[Int]]]
//   val isTree2 = implicitly[IsTree[Tree2[Int]]]
//   val isTree3 = implicitly[IsTree[Tree3[Int]]]

//   implicit class TreeOps[FA](t : FA)(implicit val isTree : IsTree[FA]) {

//     import isTree._

//     def isPoint : Boolean =
//       t match {
//         case IsPoint(a) => true
//         case _ => false
//       }

//     def visit(dir : TreeAddr[Dim]) : Option[Int] = ???

//     def seek(addr : TreeAddr[S[Dim]]) : Option[Int] = ???

//   }

//   implicit def identityIsTree[X] : IsTree[Id[X]] = 
//     new IsTree[Id[X]] {

//       type Dim = _0

//       type A = X

//       type T[+B] = Id[B]
//       type P[+B] = Nothing

//       type Dir = Nothing
//       type PrevDeriv[+B] = Unit

//       def plug(d : Derivative[A], a : A) : T[A] = a
//       def close(c : Context[A], t : T[A]) : T[A] = t

//       object IsPoint extends PointMatch {
//         def unapply(t : Id[A]) : Option[A] = Some(t)
//       }

//       object IsLeaf extends LeafMatch {
//         def unapply(t : Id[A]) : Option[Unit] = None
//       }

//       object IsBranch extends BranchMatch {
//         def unapply(t : Id[A]) : Option[(A, P[T[A]], IsTree[P[A]])] = None
//       }

//     }

//   implicit def sliceIsTree[U[+_], V](implicit ut : IsTree[U[V]]) : IsTree[Slice[U, V]] =
//     new IsTree[Slice[U, V]] {

//       type Dim = S[ut.Dim]

//       type A = V

//       type T[+X] = Slice[U, X]
//       type P[+X] = U[X]

//       type Dir = ut.Addr
//       type PrevDeriv[+X] = ut.Derivative[X]

//       def plug(d : Derivative[A], a : A) : T[A] =
//         d match {
//           case (shell, c) => close(c, Joint(a, shell))
//         }

//       def close(c : Context[A], t : T[A]) : T[A] = 
//         c match {
//           case Nil => t
//           case (a, pd) :: c => {

//             val test : PrevDeriv[T[A]] = pd
//             // ut.plug(pd, t)))

//             val utt = implicitly[IsTree[U[T[A]]]]
//             val whoops = pd.asInstanceOf[utt.Derivative[utt.A]]
//             val other = t.asInstanceOf[utt.A]
//             val almost : utt.T[utt.A] = utt.plug(whoops, other)
//             val goal : U[T[A]] = almost.asInstanceOf[U[T[A]]]

//             close(c, Joint(a, goal)) 
//           }
//         }

//       object IsPoint extends PointMatch {
//         def unapply(t : T[A]) : Option[A] = None
//       }

//       object IsLeaf extends LeafMatch {
//         def unapply(t : T[A]) : Option[Unit] =
//           t match {
//             case Cap() => Some(())
//             case _ => None
//           }
//       }

//       object IsBranch extends BranchMatch {
//         def unapply(t : T[A]) : Option[(A , U[Slice[U, A]], IsTree[P[A]])] =
//           t match {
//             case Cap() => None
//             case Joint(a, shell) => Some((a, shell, ut))
//           }
//       }

//     }
// }
