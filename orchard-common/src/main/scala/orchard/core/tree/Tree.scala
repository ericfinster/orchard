/**
  * Tree.scala - Higher Dimensional Trees
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import scala.language.higherKinds
import scala.language.implicitConversions

import scalaz.{Tree => _, Zipper => _, _}
import scalaz.std.option._
import scalaz.Leibniz._

import Nats._

sealed abstract class Dir[N <: Nat] { def dim : N }
case class Root[N <: Nat](implicit val p : N) extends Dir[S[N]] { def dim = S(p) }
case class Step[N <: Nat](d : Dir[N], ds : Dir[S[N]]) extends Dir[S[N]] { def dim = ds.dim }

object Dir {

  type Addr[N <: Nat] = Dir[S[N]]

}

sealed abstract class Tree[N <: Nat, +A] { def dim : N }
case class Pt[+A](a : A) extends Tree[_0, A] { def dim = Z }
case class Leaf[N <: Nat](addr : Dir[S[N]]) extends Tree[S[N], Nothing] { def dim = addr.dim }
case class Node[N <: Nat, +A](a : A, shell : Tree[N, Tree[S[N], A]]) extends Tree[S[N], A] { def dim = S(shell.dim) }

trait TreeFunctions { tfns => 

  import Dir._

  //============================================================================================
  // TYPE PREDICATES
  //

  def isNode[N <: Nat, A](tr : Tree[N, A]) : Boolean = rootValue(tr).isDefined
  def isLeaf[N <: Nat, A](tr : Tree[N, A]) : Boolean = ! isNode(tr)

  //============================================================================================
  // MAP
  //

  def map[N <: Nat, A, B](tr : Tree[N, A])(f : A => B) : Tree[N, B] = 
    MapRecursor.execute(tr.dim)(tr, f)

  type MapIn0[M <: Nat, A, B] = Tree[M, A]
  type MapIn1[M <: Nat, A, B] = A => B
  type MapOut[M <: Nat, A, B] = Tree[M, B]

  object MapRecursor extends NatRecursorT2P2[MapIn0, MapIn1, MapOut] {

    def caseZero[A, B](tr : Tree[_0, A], f : A => B) : Tree[_0, B] =
      tr match {
        case Pt(a) => Pt(f(a))
      }

    def caseSucc[P <: Nat, A, B](tr : Tree[S[P], A], f : A => B) : Tree[S[P], B] = 
      tr match {
        case Leaf(addr) => Leaf(addr)
        case Node(a, shell) => Node(f(a), map(shell)(map(_)(f))) 
      }

  }

  //============================================================================================
  // TRAVERSE
  //

  def traverse[N <: Nat, G[_], A, B](tr : Tree[N, A])(f : A => G[B])(implicit apG : Applicative[G]) : G[Tree[N, B]] = 
    TraverseRecursor.execute(tr.dim)(tr, f, apG)

  type TraverseIn0[N <: Nat, G[_], A, B] = Tree[N, A]
  type TraverseIn1[N <: Nat, G[_], A, B] = A => G[B]
  type TraverseIn2[N <: Nat, G[_], A, B] = Applicative[G]
  type TraverseOut[N <: Nat, G[_], A, B] = G[Tree[N, B]]

  object TraverseRecursor extends NatRecursorC1T2P3[TraverseIn0, TraverseIn1, TraverseIn2, TraverseOut] {

    def caseZero[G[_], A, B](tr : Tree[_0, A], f : A => G[B], apG : Applicative[G]) : G[Tree[_0, B]] = {
      import apG._

      tr match {
        case Pt(a) => ap(f(a))(pure((b : B) => Pt(b)))
      }
    }

    def caseSucc[P <: Nat, G[_], A, B](tr : Tree[S[P], A], f : A => G[B], apG : Applicative[G]) : G[Tree[S[P], B]] = {
      import apG._

      tr match {
        case l @ Leaf(addr) => pure(l)
        case Node(a, sh) => {

          val nodeCons : G[(B , Tree[P, Tree[S[P], B]]) => Tree[S[P], B]] =
            pure((b : B, sh : Tree[P, Tree[S[P], B]]) => Node(b, sh))

          ap2(f(a), tfns.traverse(sh)(caseSucc(_, f, apG))(apG))(nodeCons)

        }
      }
    }

  }

  //============================================================================================
  // CUSTOM TRAVERSALS
  //

  // Ugggh.  Can we do better with the traversal implicit ... ???

  def nodesOf[N <: Nat, A](tr : Tree[N, A]) : List[A] = 
    implicitly[Traverse[({ type L[+X] = Tree[N, X] })#L]].toList(tr)

  def nodeCountOf[N <: Nat, A](tr : Tree[N, A]) : Int = 
    implicitly[Traverse[({ type L[+X] = Tree[N, X] })#L]].count(tr)

  def zipWithIndex[N <: Nat, A](tr : Tree[N, A]) : (Int, Tree[N, (A, Int)]) = 
    implicitly[Traverse[({ type L[+X] = Tree[N, X] })#L]].
      mapAccumL(tr, 0)((i : Int, a : A) => (i + 1, (a, i)))

  //============================================================================================
  // GRAFT
  //

  def graft[N <: Nat, A](tr : Tree[S[N], A], brs : Tree[N, Tree[S[N], A]]) : Option[Tree[S[N], A]] = 
    tr match {
      case Leaf(addr) => valueAt(brs, addr)
      case Node(a, sh) =>
        for {
          nsh <- traverse(sh)(graft(_, brs))
        } yield Node(a, nsh)
    }

  //============================================================================================
  // JOIN
  //

  def join[N <: Nat, A](tr : Tree[N, Tree[N, A]]) : Option[Tree[N, A]] =  
    JoinRecursor.execute(tr.dim)(tr)

  type JoinIn[M <: Nat, A] = Tree[M, Tree[M, A]]
  type JoinOut[M <: Nat, A] = Option[Tree[M, A]]

  object JoinRecursor extends NatRecursorT1P1[JoinIn, JoinOut] {

    def caseZero[A](tr : Tree[_0, Tree[_0, A]]) : Option[Tree[_0, A]] = 
      rootValue(tr)

    def caseSucc[P <: Nat, A](tr : Tree[S[P], Tree[S[P], A]]) : Option[Tree[S[P], A]] =
      tr match {
        case Leaf(addr) => Some(Leaf(addr))
        case Node(t, tsh) =>
          for {
            gsh <- traverse(tsh)(caseSucc(_))
            str <- graft(t, gsh)
          } yield str
      }

  }

  //============================================================================================
  // SEEK
  //

  def seek[N <: Nat, A](tr : Tree[N, A], addr : Addr[N]) : Option[Zipper[N, A]] = 
    SeekRecursor.execute(tr.dim)(tr, addr)

  type SeekIn0[N <: Nat, A] = Tree[N, A]
  type SeekIn1[N <: Nat, A] = Addr[N]
  type SeekOut[N <: Nat, A] = Option[Zipper[N, A]]

  object SeekRecursor extends NatOneRecursorT1P2[SeekIn0, SeekIn1, SeekOut] {

    def caseZero[A](tr : Tree[_0, A], addr : Addr[_0]) : Option[Zipper[_0, A]] = 
      Zipper.seek(addr, FocusPoint(tr))

    def caseOne[A](tr : Tree[_1, A], addr : Addr[_1]) : Option[Zipper[_1, A]] = 
      Zipper.seek(addr, FocusList(tr, Empty()))

    def caseDblSucc[P <: Nat, A](tr : Tree[S[S[P]], A], addr : Addr[S[S[P]]]) : Option[Zipper[S[S[P]], A]] = 
      Zipper.seek(addr, FocusBranch(tr, Empty()))

  }

  //============================================================================================
  // VALUE AT
  //

  def valueAt[N <: Nat, A](tr : Tree[N, A], addr : Addr[N]) : Option[A] = 
    for {
      zp <- seek(tr, addr)
      a <- rootValue(zp.focus)
    } yield a

  //============================================================================================
  // ROOT VALUE
  //

  def rootValue[N <: Nat, A](tr : Tree[N, A]) : Option[A] = 
    RootValueRecursor.execute(tr.dim)(tr)

  type RootValueIn[N <: Nat, A] = Tree[N, A]
  type RootValueOut[N <: Nat, A] = Option[A]

  object RootValueRecursor extends NatRecursorT1P1[RootValueIn, RootValueOut] {

    def caseZero[A](tr : Tree[_0, A]) : Option[A] = 
      tr match {
        case Pt(a) => Some(a)
      }

    def caseSucc[P <: Nat, A](tr : Tree[S[P], A]) : Option[A] = 
      tr match {
        case Leaf(_) => None
        case Node(a, _) => Some(a)
      }

  }

  //============================================================================================
  // ZIP COMPLETE
  //

  def zipComplete[N <: Nat, A, B](trA : Tree[N, A], trB : Tree[N, B]) : Option[Tree[N, (A, B)]] = 
    ZipCompleteRecursor.execute(trA.dim)(trA, trB)

  type ZipCompleteIn0[N <: Nat, A, B] = Tree[N, A]
  type ZipCompleteIn1[N <: Nat, A, B] = Tree[N, B]
  type ZipCompleteOut[N <: Nat, A, B] = Option[Tree[N, (A, B)]]

  object ZipCompleteRecursor extends NatRecursorT2P2[ZipCompleteIn0, ZipCompleteIn1, ZipCompleteOut] {

    def caseZero[A, B](trA : Tree[_0, A], trB : Tree[_0, B]) : Option[Tree[_0, (A, B)]] = 
      (trA, trB) match {
        case (Pt(a), Pt(b)) => Some(Pt((a, b)))
      }

    def caseSucc[P <: Nat, A, B](trA : Tree[S[P], A], trB : Tree[S[P], B]) : Option[Tree[S[P], (A, B)]] = 
      (trA, trB) match {
        case (Leaf(addr), Leaf(_)) => Some(Leaf(addr))
        case (Node(a, ash), Node(b, bsh)) => 
          for {
            zsh <- zipComplete(ash, bsh)
            psh <- traverse(zsh)({
              case (at, bt) => caseSucc(at, bt)
            })
          } yield Node((a, b), psh)

        case _ => None
      }

  }

  //============================================================================================
  // UNZIP
  //

  def unzip[N <: Nat, A, B](tr : Tree[N, (A, B)]) : (Tree[N, A], Tree[N, B]) = 
    UnzipRecursor.execute(tr.dim)(tr)

  type UnzipIn[M <: Nat, A, B] = Tree[M, (A, B)]
  type UnzipOut[M <: Nat, A, B] = (Tree[M, A], Tree[M, B])

  object UnzipRecursor extends NatRecursorT2P1[UnzipIn, UnzipOut] {

    def caseZero[A, B](tr : Tree[_0, (A, B)]) : (Tree[_0, A], Tree[_0, B]) = 
      tr match {
        case Pt((a, b)) => (Pt(a), Pt(b))
      }

    def caseSucc[P <: Nat, A, B](tr : Tree[S[P], (A, B)]) : (Tree[S[P], A], Tree[S[P], B]) = 
      tr match {
        case Leaf(addr) => (Leaf(addr), Leaf(addr))
        case Node((a, b), shell) => {
          val (ash, bsh) = unzip(map(shell)(unzip(_)))
          (Node(a, ash), Node(b, bsh))
        }
      }

  }

}

object Tree extends TreeFunctions { 

  implicit def treeIsTraverse[N <: Nat] : Traverse[({ type L[+A] = Tree[N, A] })#L] = 
    new Traverse[({ type L[+A] = Tree[N, A] })#L] {

      def traverseImpl[G[_], A, B](ta : Tree[N, A])(f : A => G[B])(implicit isA : Applicative[G]) : G[Tree[N, B]] = 
        Tree.traverse(ta)(f)

    }

}
