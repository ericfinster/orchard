/**
  * Nesting.scala - Nestings
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

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

object Nesting {

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
        case Dot(a, srcs) => Some(Node(a, map(srcs)(Leaf(_))))
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
          Box((a, addr), map(Tree.zipWithAddress(canopy))({
            case (t, d) => withAddressPrefix(Step(d, addr), t)
          }))
        }
      }

    def caseSucc[P <: Nat, A](addr : Address[S[S[P]]], nst : Nesting[S[P], A]) : Nesting[S[P], (A, Address[S[S[P]]])] = 
      nst match {
        case Dot(a, cor) => Dot((a, addr), cor)
        case Box(a, canopy) => {
          Box((a, addr), map(Tree.zipWithAddress(canopy))({
            case (t, d) => withAddressPrefix(Step(d, addr), t)
          }))
        }
      }

  }

  def withAddress[N <: Nat, A](nst : Nesting[N, A]) : Nesting[N, (A, Address[S[N]])] = 
    withAddressPrefix(Root()(S(nst.dim)), nst)

  //============================================================================================
  // WITH FACE ADDRESS
  //

  // def withFaceAddressPrefix[N <: Nat, M <: Nat, A](addr : Address[S[M]], nst : Nesting[M, A], gte : Gte[N, M]) : Nesting[M, (A, FaceAddress[N])] = 
    ???
    // WithAddrRecursor.execute(nst.dim)(addr, nst)

  // type WithAddrPrefIn0[N <: Nat, A] = Address[S[N]]
  // type WithAddrPrefIn1[N <: Nat, A] = Nesting[N, A]
  // type WithAddrPrefOut[N <: Nat, A] = Nesting[N, (A, Address[S[N]])]


  // object WithAddrRecursor extends NatRecursorT1P2[WithAddrPrefIn0, WithAddrPrefIn1, WithAddrPrefOut] {

  //   def caseZero[A](addr : Address[_1], nst : Nesting[_0, A]) : Nesting[_0, (A, Address[_1])] =
  //     nst match {
  //       case Obj(a) => Obj((a, addr))
  //       case Box(a, canopy) => {
  //         Box((a, addr), map(Tree.zipWithAddress(canopy))({
  //           case (t, d) => withAddressPrefix(Step(d, addr), t)
  //         }))
  //       }
  //     }

  //   def caseSucc[P <: Nat, A](addr : Address[S[S[P]]], nst : Nesting[S[P], A]) : Nesting[S[P], (A, Address[S[S[P]]])] = 
  //     nst match {
  //       case Dot(a, cor) => Dot((a, addr), cor)
  //       case Box(a, canopy) => {
  //         Box((a, addr), map(Tree.zipWithAddress(canopy))({
  //           case (t, d) => withAddressPrefix(Step(d, addr), t)
  //         }))
  //       }
  //     }

  // }

  // def withAddress[N <: Nat, A](nst : Nesting[N, A]) : Nesting[N, (A, Address[S[N]])] = 
  //   withAddressPrefix(Root()(S(nst.dim)), nst)

}
