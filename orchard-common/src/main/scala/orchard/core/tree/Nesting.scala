/**
  * Nesting.scala - Nestings
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import scala.language.higherKinds
import scala.language.implicitConversions

import scalaz.{Tree => _, Zipper => _, _}
import scalaz.std.option._

import Nats._
import Tree._
import Suite._
import Complex._

sealed abstract class Nesting[N <: Nat, +A] { def dim : N }
case class Obj[+A](a : A) extends Nesting[_0, A] { def dim = Z }
case class Dot[N <: Nat, +A](a : A, c : Tree[N, Address[N]]) extends Nesting[S[N], A] { def dim = S(c.dim) }
case class Box[N <: Nat, +A](a : A, c : Tree[N, Nesting[N, A]]) extends Nesting[N, A] { def dim = c.dim }

trait NestingFunctions {

  //============================================================================================
  // MAP
  //

  def mapNesting[N <: Nat, A, B](nst : Nesting[N, A])(f : A => B) : Nesting[N, B] = 
    MapRecursor.execute(nst.dim)(nst, f)

  type MapIn0[M <: Nat, A, B] = Nesting[M, A]
  type MapIn1[M <: Nat, A, B] = A => B
  type MapOut[M <: Nat, A, B] = Nesting[M, B]

  object MapRecursor extends NatRecursorT2P2[MapIn0, MapIn1, MapOut] {

    def caseZero[A, B](nst : Nesting[_0, A], f : A => B) : Nesting[_0, B] = 
      nst match {
        case Obj(a) => Obj(f(a))
        case Box(a, c) => Box(f(a), c map (mapNesting(_)(f)))
      }

    def caseSucc[P <: Nat, A, B](nst : Nesting[S[P], A], f : A => B) : Nesting[S[P], B] = 
      nst match {
        case Dot(a, c) => Dot(f(a), c)
        case Box(a, c) => Box(f(a), c map (mapNesting(_)(f)))
      }
  }

  //============================================================================================
  // TRAVERSE
  //

  def traverseNesting[N <: Nat, G[_], A, B](nst : Nesting[N, A])(f : A => G[B])(implicit apG : Applicative[G]) : G[Nesting[N, B]] = 
    TraverseRecursor.execute(nst.dim)(nst, f, apG)

  type TraverseIn0[N <: Nat, G[_], A, B] = Nesting[N, A]
  type TraverseIn1[N <: Nat, G[_], A, B] = A => G[B]
  type TraverseIn2[N <: Nat, G[_], A, B] = Applicative[G]
  type TraverseOut[N <: Nat, G[_], A, B] = G[Nesting[N, B]]

  object TraverseRecursor extends NatRecursorC1T2P3[TraverseIn0, TraverseIn1, TraverseIn2, TraverseOut] {

    def caseZero[G[_], A, B](nst : Nesting[_0, A], f : A => G[B], apG : Applicative[G]) : G[Nesting[_0, B]] = {
      import apG._

      nst match {
        case Obj(a) => ap(f(a))(pure((b : B) => Obj(b)))
        case Box(a, c) => ap2(f(a), Tree.traverse(c)(caseZero(_, f, apG))(apG))(
          pure((b : B, cn : Tree[_0, Nesting[_0, B]]) => Box(b, cn))
        )
      }
    }

    def caseSucc[P <: Nat, G[_], A, B](nst : Nesting[S[P], A], f : A => G[B], apG : Applicative[G]) : G[Nesting[S[P], B]] = {
      import apG._

      nst match {
        case Dot(a, c) => ap2(f(a), pure(c))(pure((b : B, cr : Tree[P, Address[P]]) => Dot(b, cr)))
        case Box(a, c) => ap2(f(a), Tree.traverse(c)(caseSucc(_, f, apG))(apG))(
          pure((b : B, cn : Tree[S[P], Nesting[S[P], B]]) => Box(b, cn))
        )
      }
    }

  }

  //============================================================================================
  // LABEL OF
  //

  def labelOf[N <: Nat, A](nst : Nesting[N, A]) : A = 
    LabelRecursor.execute(nst.dim)(nst)

  type LabelIn[N <: Nat, A] = Nesting[N, A]
  type LabelOut[N <: Nat, A] = A

  object LabelRecursor extends NatRecursorT1P1[LabelIn, LabelOut] {

    def caseZero[A](nst : Nesting[_0, A]) : A = 
      nst match {
        case Obj(a) => a
        case Box(a, _) => a
      }

    def caseSucc[P <: Nat, A](nst : Nesting[S[P], A]) : A = 
      nst match {
        case Dot(a, _) => a
        case Box(a, _) => a
      }

  }

  //============================================================================================
  // SPINE
  //

  def spine[N <: Nat, A](nst : Nesting[N, A]) : Option[Tree[N, A]] = 
    SpineRecursor.execute(nst.dim)(nst)

  type SpineIn[N <: Nat, A] = Nesting[N, A]
  type SpineOut[N <: Nat, A] = Option[Tree[N, A]]

  object SpineRecursor extends NatRecursorT1P1[SpineIn, SpineOut] {

    def caseZero[A](nst : Nesting[_0, A]) : Option[Tree[_0, A]] = 
      nst match {
        case Obj(a) => Some(Pt(a))
        case Box(a, Pt(n)) => caseZero(n)
      }

    def caseSucc[P <: Nat, A](nst : Nesting[S[P], A]) : Option[Tree[S[P], A]] = 
      nst match {
        case Dot(a, srcs) => Some(Node(a, srcs map (Leaf(_)))) 
        case Box(a, canopy) => 
          for {
            st <- traverse(canopy)(caseSucc(_))
            sp <- join(st)
          } yield sp
      }

  }

  //============================================================================================
  // WITH ADDRESS
  //

  def withAddressPrefix[N <: Nat, A](addr : Address[S[N]], nst : Nesting[N, A]) : Nesting[N, (A, Address[S[N]])] = 
    WithAddrRecursor.execute(nst.dim)(addr, nst)

  type WithAddrPrefIn0[N <: Nat, A] = Address[S[N]]
  type WithAddrPrefIn1[N <: Nat, A] = Nesting[N, A]
  type WithAddrPrefOut[N <: Nat, A] = Nesting[N, (A, Address[S[N]])]


  object WithAddrRecursor extends NatRecursorT1P2[WithAddrPrefIn0, WithAddrPrefIn1, WithAddrPrefOut] {

    def caseZero[A](addr : Address[_1], nst : Nesting[_0, A]) : Nesting[_0, (A, Address[_1])] =
      nst match {
        case Obj(a) => Obj((a, addr))
        case Box(a, canopy) => {
          Box((a, addr), canopy.zipWithAddress map ({     
            case (t, d) => withAddressPrefix(Step(d, addr), t)
          }))
        }
      }

    def caseSucc[P <: Nat, A](addr : Address[S[S[P]]], nst : Nesting[S[P], A]) : Nesting[S[P], (A, Address[S[S[P]]])] = 
      nst match {
        case Dot(a, cor) => Dot((a, addr), cor)
        case Box(a, canopy) => {
          Box((a, addr), canopy.zipWithAddress map ({ 
            case (t, d) => withAddressPrefix(Step(d, addr), t)
          }))
        }
      }

  }

  def withAddress[N <: Nat, A](nst : Nesting[N, A]) : Nesting[N, (A, Address[S[N]])] = 
    withAddressPrefix(Root()(S(nst.dim)), nst)

  //============================================================================================
  // ZIP COMPLETE
  //

  def zipComplete[N <: Nat, A, B](nstA : Nesting[N, A], nstB : Nesting[N, B]) : Option[Nesting[N, (A, B)]] = 
    ZipCompleteRecursor.execute(nstA.dim)(nstA, nstB)

  type ZipCompleteIn0[N <: Nat, A, B] = Nesting[N, A]
  type ZipCompleteIn1[N <: Nat, A, B] = Nesting[N, B]
  type ZipCompleteOut[N <: Nat, A, B] = Option[Nesting[N, (A, B)]]

  object ZipCompleteRecursor extends NatRecursorT2P2[ZipCompleteIn0, ZipCompleteIn1, ZipCompleteOut] {

    def caseZero[A, B](nstA : Nesting[_0, A], nstB : Nesting[_0, B]) : Option[Nesting[_0, (A, B)]] = 
      (nstA, nstB) match {
        case (Obj(a), Obj(b)) => Some(Obj((a, b)))
        case (Box(a, cpA), Box(b, cpB)) => 
          for {
            cpAB <- cpA matchWith cpB
            cpRes <- cpAB traverse {
              case (nA, nB) => caseZero(nA, nB)
            }
          } yield Box((a, b), cpRes)
        case _ => None
      }

    def caseSucc[P <: Nat, A, B](nstA : Nesting[S[P], A], nstB : Nesting[S[P], B]) : Option[Nesting[S[P], (A, B)]] = 
      (nstA, nstB) match {
        case (Dot(a, crA), Dot(b, crB)) => Some(Dot((a, b), crA))
        case (Box(a, cpA), Box(b, cpB)) => 
          for {
            cpAB <- cpA matchWith cpB
            cpRes <- cpAB traverse {
              case (nA, nB) => caseSucc(nA, nB)
            }
          } yield Box((a, b), cpRes)
        case _ => None
      }

  }


}

object Nesting extends NestingFunctions {

  implicit def nestingIsTraverse[N <: Nat] : Traverse[({ type L[+A] = Nesting[N, A] })#L] = 
    new Traverse[({ type L[+A] = Nesting[N, A] })#L] {

      override def map[A, B](na : Nesting[N, A])(f : A => B) : Nesting[N, B] = 
        mapNesting(na)(f)

      def traverseImpl[G[_], A, B](na : Nesting[N, A])(f : A => G[B])(implicit isA : Applicative[G]) : G[Nesting[N, B]] = 
        traverseNesting(na)(f)

    }


  import scalaz.syntax.FunctorOps
  import scalaz.syntax.functor._

  implicit def nestingToFunctorOps[N <: Nat, A](nst : Nesting[N, A]) : FunctorOps[({ type L[+X] = Nesting[N, X] })#L, A] = 
    ToFunctorOps[({ type L[+X] = Nesting[N, X] })#L, A](nst)

  import scalaz.syntax.TraverseOps
  import scalaz.syntax.traverse._

  implicit def nestingToTraverseOps[N <: Nat, A](nst : Nesting[N, A]) : TraverseOps[({ type L[+X] = Nesting[N, X] })#L, A] = 
    ToTraverseOps[({ type L[+X] = Nesting[N, X] })#L, A](nst)

  class NestingOps[N <: Nat, A](nst : Nesting[N, A]) {

    def zipWithAddress : Nesting[N, (A, Address[S[N]])] = 
      withAddress(nst)

    def matchWith[B](nstB : Nesting[N, B]) : Option[Nesting[N, (A, B)]] = 
      Nesting.zipComplete(nst, nstB)

  }

  implicit def nestingToNestinOps[N <: Nat, A](nst : Nesting[N, A]) : NestingOps[N, A] = 
    new NestingOps[N, A](nst)

}
