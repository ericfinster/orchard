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
    MapRecursor.recurseWith(tr, f)

  type MapIn[M <: Nat, A, B] = A => B
  type MapOut[M <: Nat, A, B] = Tree[M, B]

  object MapRecursor extends TreeRecursorT2P1[MapIn, MapOut] {

    def zeroTree[A, B](tr : Tree[_0, A])(f : A => B) : Tree[_0, B] =
      tr match {
        case Pt(a) => Pt(f(a))
      }

    def succTree[P <: Nat, A, B](tr : Tree[S[P], A])(f : A => B) : Tree[S[P], B] = 
      tr match {
        case Leaf(addr) => Leaf(addr)
        case Node(a, shell) => Node(f(a), map(shell)(map(_)(f))) 
      }

  }

  //============================================================================================
  // TRAVERSE
  //

  def traverse[N <: Nat, G[_], A, B](tr : Tree[N, A])(f : A => G[B])(implicit apG : Applicative[G]) : G[Tree[N, B]] = 
    TraverseRecursor.recurseWith(tr)(f, apG)

  type TraverseIn[N <: Nat, G[_], A, B] = A => G[B]
  type TraverseImpl[N <: Nat, G[_], A, B] = Applicative[G]
  type TraverseOut[N <: Nat, G[_], A, B] = G[Tree[N, B]]

  object TraverseRecursor extends TreeRecursorC1T2P2[TraverseIn, TraverseImpl, TraverseOut] {

    def zeroTree[G[_], A, B](tr : Tree[_0, A])(f : A => G[B], apG : Applicative[G]) : G[Tree[_0, B]] = {
      import apG._

      tr match {
        case Pt(a) => ap(f(a))(pure((b : B) => Pt(b)))
      }
    }

    def succTree[P <: Nat, G[_], A, B](tr : Tree[S[P], A])(f : A => G[B], apG : Applicative[G]) : G[Tree[S[P], B]] = {
      import apG._

      tr match {
        case l @ Leaf(addr) => pure(l)
        case Node(a, sh) => {

          val nodeCons : G[(B , Tree[P, Tree[S[P], B]]) => Tree[S[P], B]] =
            pure((b : B, sh : Tree[P, Tree[S[P], B]]) => Node(b, sh))

          ap2(f(a), tfns.traverse(sh)(succTree(_)(f, apG))(apG))(nodeCons)

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
    natRecT1P1(tr.dim)(JoinRecursor)(tr)

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
    natOneRecT1P2(tr.dim)(SeekRecursor)(tr, addr)

  type SeekIn0[N <: Nat, A] = Tree[N, A]
  type SeekIn1[N <: Nat, A] = Addr[N]
  type SeekOut[N <: Nat, A] = Option[Zipper[N, A]]

  object SeekRecursor extends NatOneRecursorT1P2[SeekIn0, SeekIn1, SeekOut] {

    def caseZero[A](tr : Tree[_0, A], addr : Addr[_0]) : Option[Zipper[_0, A]] = ???
    def caseOne[A](tr : Tree[_1, A], addr : Addr[_1]) : Option[Zipper[_1, A]] = ???
    def caseDblSucc[P <: Nat, A](tr : Tree[S[S[P]], A], addr : Addr[S[S[P]]]) : Option[Zipper[S[S[P]], A]] = ???

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
    natRecT1P1(tr.dim)(RootValueRecursor)(tr)

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
    ZipCompleteRecursor.recurseWith(trA, trB)

  type ZipCompleteIn[N <: Nat, A, B] = Tree[N, B]
  type ZipCompleteOut[N <: Nat, A, B] = Option[Tree[N, (A, B)]]

  object ZipCompleteRecursor extends TreeRecursorT2P1[ZipCompleteIn, ZipCompleteOut] {

    def zeroTree[A, B](trA : Tree[_0, A])(trB : Tree[_0, B]) : Option[Tree[_0, (A, B)]] = 
      (trA, trB) match {
        case (Pt(a), Pt(b)) => Some(Pt((a, b)))
      }

    def succTree[P <: Nat, A, B](trA : Tree[S[P], A])(trB : Tree[S[P], B]) : Option[Tree[S[P], (A, B)]] = 
      (trA, trB) match {
        case (Leaf(addr), Leaf(_)) => Some(Leaf(addr))
        case (Node(a, ash), Node(b, bsh)) => 
          for {
            zsh <- zipComplete(ash, bsh)
            psh <- traverse(zsh)({
              case (at, bt) => succTree(at)(bt)
            })
          } yield Node((a, b), psh)

        case _ => None
      }

  }

  //============================================================================================
  // UNZIP
  //

  def unzip[N <: Nat, A, B](tr : Tree[N, (A, B)]) : (Tree[N, A], Tree[N, B]) = 
    natRecT2P1(tr.dim)(UnzipRecursor)(tr)

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

  //============================================================================================
  // TREE RECURSOR DEFINITIONS
  //

  // I thought this specialization would be useful, but I think it actually makes things a bit less
  // uniform.  Probably you should take it out and just beef up the number of nat implementations ...

  abstract class TreeRecursorC1T2P2[F[_ <: Nat, _[_], _, _], G[_ <: Nat, _[_], _, _], H[_ <: Nat, _[_], _, _]] 
      extends NatRecursorC1T2P1[({ type L[M <: Nat, T[_], A, B] = (Tree[M, A], F[M, T, A, B], G[M, T, A, B]) })#L, H] {

    type In[M <: Nat, T[_], A, B] = (Tree[M, A], F[M, T, A, B], G[M, T, A, B])
    type Out[M <: Nat, T[_], A, B] = H[M, T, A, B]

    def zeroTree[T[_], A, B](tr : Tree[_0, A])(f : F[_0, T, A, B], g : G[_0, T, A, B]) : H[_0, T, A, B]
    def succTree[P <: Nat, T[_], A, B](tr : Tree[S[P], A])(f : F[S[P], T, A, B], g : G[S[P], T, A, B]) : H[S[P], T, A, B]

    def caseZero[T[_], A, B](trpl : (Tree[_0, A], F[_0, T, A, B], G[_0, T, A, B])) : H[_0, T, A, B] = 
      zeroTree(trpl._1)(trpl._2, trpl._3)

    def caseSucc[P <: Nat, T[_], A, B](trpl : (Tree[S[P], A], F[S[P], T, A, B], G[S[P], T, A, B])) : H[S[P], T, A, B] =
      succTree(trpl._1)(trpl._2, trpl._3)

    def recurseWith[N <: Nat, T[_], A, B](tr : Tree[N, A])(f : F[N, T, A, B], g : G[N, T, A, B]) : H[N, T, A, B] =
      natRecC1T2P1[In, Out, N, T, A, B](tr.dim)(this)((tr, f, g))

  }

  abstract class TreeRecursorT1P1[F[_ <: Nat, _], G[_ <: Nat, _]] 
      extends NatRecursorT1P1[({ type L[M <: Nat, A] = (Tree[M, A], F[M, A]) })#L, G] {

    type In[M <: Nat, A] = (Tree[M, A], F[M, A])
    type Out[M <: Nat, A] = G[M, A]

    def zeroTree[A](tr : Tree[_0, A])(f : F[_0, A]) : G[_0, A]
    def succTree[P <: Nat, A](tr : Tree[S[P], A])(f : F[S[P], A]) : G[S[P], A]

    def caseZero[A](pr : (Tree[_0, A], F[_0, A])) : G[_0, A] = zeroTree(pr._1)(pr._2)
    def caseSucc[P <: Nat, A](pr : (Tree[S[P], A], F[S[P], A])) : G[S[P], A] = succTree(pr._1)(pr._2)

    def recurseWith[N <: Nat, A](tr : Tree[N, A], f : F[N, A]) : G[N, A] =
      natRecT1P1[In, Out, N, A](tr.dim)(this)((tr, f))

  }

  abstract class TreeRecursorT2P1[F[_ <: Nat, _, _], G[_ <: Nat, _, _]] 
      extends NatRecursorT2P1[({ type L[M <: Nat, A, B] = (Tree[M, A], F[M, A, B]) })#L, G] {

    type In[M <: Nat, A, B] = (Tree[M, A], F[M, A, B])
    type Out[M <: Nat, A, B] = G[M, A, B]

    def zeroTree[A, B](tr : Tree[_0, A])(f : F[_0, A, B]) : G[_0, A, B]
    def succTree[P <: Nat, A, B](tr : Tree[S[P], A])(f : F[S[P], A, B]) : G[S[P], A, B]

    def caseZero[A, B](pr : (Tree[_0, A], F[_0, A, B])) : G[_0, A, B] = zeroTree(pr._1)(pr._2)
    def caseSucc[P <: Nat, A, B](pr : (Tree[S[P], A], F[S[P], A, B])) : G[S[P], A, B] = succTree(pr._1)(pr._2)

    def recurseWith[N <: Nat, A, B](tr : Tree[N, A], f : F[N, A, B]) : G[N, A, B] =
      natRecT2P1[In, Out, N, A, B](tr.dim)(this)((tr, f))

  }

}

object Tree extends TreeFunctions { 

  implicit def treeIsTraverse[N <: Nat] : Traverse[({ type L[+A] = Tree[N, A] })#L] = 
    new Traverse[({ type L[+A] = Tree[N, A] })#L] {

      def traverseImpl[G[_], A, B](ta : Tree[N, A])(f : A => G[B])(implicit isA : Applicative[G]) : G[Tree[N, B]] = 
        Tree.traverse(ta)(f)

    }

}
