/**
  * NestingZipper.scala - Zippers for Nestings
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import Nats._
import Tree._
import Nesting._

//============================================================================================
// NESTING DERIVATIVES
//

case class NestingDerivative[N <: Nat, +A](val canopy : Tree[N, Nesting[N, A]], val context : NestingContext[N, A]) {

  def plugWith[B >: A](b : B) : Nesting[N, B] = context.closeWith(Box(b, canopy))

}

//============================================================================================
// NESTING CONTEXTS
//

sealed abstract class NestingContext[N <: Nat, +A] {

  def closeWith[B >: A](nst : Nesting[N, B]) : Nesting[N, B] = 
    this match {
      case Bottom() => nst
      case Select(a, d, c) => c.closeWith(Box(a, Derivative.plug(d, nst)))
    }

}

case class Bottom[N <: Nat]() extends NestingContext[N, Nothing]
case class Select[N <: Nat, +A](a : A, d : Derivative[N, Nesting[N, A]], c : NestingContext[N, A]) extends NestingContext[N, A]

//============================================================================================
// NESTING ZIPPERS
//

case class NestingZipper[N <: Nat, +A](val focus : Nesting[N, A], val context : NestingContext[N, A]) {

  def dim : N = focus.dim

}

object NestingZipper {

  //============================================================================================
  // VISIT
  //

  def visit[N <: Nat, A](dir : Direction[S[N]], nz : NestingZipper[N, A]) : Option[NestingZipper[N, A]] = 
    VisitRecursor.execute(nz.dim)(dir, nz)

  type VisitIn0[N <: Nat, A] = Direction[S[N]]
  type VisitIn1[N <: Nat, A] = NestingZipper[N, A]
  type VisitOut[N <: Nat, A] = Option[NestingZipper[N, A]]

  object VisitRecursor extends NatRecursorT1P2[VisitIn0, VisitIn1, VisitOut] {

    def caseZero[A](dir : Direction[_1], nz : NestingZipper[_0, A]) = 
      nz match {
        case NestingZipper(Obj(_), cntxt) => None
        case NestingZipper(Box(a, Pt(int)), cntxt) => 
          Some(NestingZipper(int, Select(a, ZeroDeriv, cntxt)))
      }

    def caseSucc[P <: Nat, A](dir : Direction[S[S[P]]], nz : NestingZipper[S[P], A]) : Option[NestingZipper[S[P], A]] = 
      nz match {
        case NestingZipper(Dot(_, _), cntxt) => None
        case NestingZipper(Box(a, canopy), cntxt) => 
          for {
            loc <- Tree.seek(canopy, dir)
            res <- (
              loc.focus match {
                case Leaf(_) => None
                case Node(nst, hsh) => 
                  Some(NestingZipper(nst, Select(a, Open(hsh, loc.context), cntxt)))
              }
            )
          } yield res
      }

  }

  //============================================================================================
  // SIBLING
  //

  def sibling[N <: Nat, A](addr : Address[N], nz : NestingZipper[S[N], A]) : Option[NestingZipper[S[N], A]] =
    SiblingRecursor.execute(addr.dim)(addr, nz)

  type SiblingIn0[N <: Nat, A] = Address[N]
  type SiblingIn1[N <: Nat, A] = NestingZipper[S[N], A]
  type SiblingOut[N <: Nat, A] = Option[NestingZipper[S[N], A]]

  object SiblingRecursor extends NatRecursorT1P2[SiblingIn0, SiblingIn1, SiblingOut] {

    def caseZero[A](addr : Address[_0], nz : NestingZipper[_1, A]) : Option[NestingZipper[_1, A]] = 
      nz match {
        case NestingZipper(fcs, Bottom()) => None
        case NestingZipper(fcs, Select(a, Open(Pt(Leaf(_)), hcn), c)) => None
        case NestingZipper(fcs, Select(a, Open(Pt(Node(nfcs, shell)), hcn), c)) => 
          Some(NestingZipper(nfcs, Select(a, Open(shell, Then(fcs, ZeroDeriv, hcn)), c)))
      }

    def caseSucc[P <: Nat, A](addr : Address[S[P]], nz : NestingZipper[S[S[P]], A]) : Option[NestingZipper[S[S[P]], A]] = 
      nz match {
        case NestingZipper(fcs, Bottom()) => None
        case NestingZipper(fcs, Select(a, Open(verts, hcn), c)) => 
          for {
            vzip <- Tree.seek(verts, addr)
            res <- (
              vzip.focus match {
                case Leaf(_) => None
                case Node(Leaf(_), _) => None
                case Node(Node(nfcs, vrem), hmask) =>
                  Some(NestingZipper(nfcs, Select(a, Open(vrem, Then(fcs, Open(hmask, vzip.context), hcn)), c)))
              }
            )
          } yield res
      }

  }

  //============================================================================================
  // SEEK
  //

  def seek[N <: Nat, A](addr : Address[S[N]], nz : NestingZipper[N, A]) : Option[NestingZipper[N, A]] = 
    addr match {
      case Root() => Some(nz)
      case Step(d, ds) =>
        for {
          zp <- seek(ds, nz)
          zr <- visit(d, zp)
        } yield zr
    }

}


