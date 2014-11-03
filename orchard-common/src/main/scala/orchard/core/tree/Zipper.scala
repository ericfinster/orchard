/**
  * Zipper.scala - Zippers for Higher Dimensional Trees
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import scalaz.Leibniz._

import Dir._
import Nats._
import Tree._

//============================================================================================
// Derivatives
//

sealed abstract class Derivative[N <: Nat, +A] 
case object ZeroDeriv extends Derivative[_0, Nothing] 
case class Open[N <: Nat, +A](sh : Tree[N, Tree[S[N], A]], ct : Context[S[N], A]) extends Derivative[S[N], A]

object Derivative {

  //============================================================================================
  // PLUG
  //

  def plug[N <: Nat, A](d : Derivative[N, A], a : A) : Tree[N, A] = ???

  type PlugIn0[N <: Nat, A] = Derivative[N, A]
  type PlugIn1[N <: Nat, A] = A
  type PlugOut[N <: Nat, A] = Tree[N, A]

  object PlugRecursor extends NatRecursorT1P2[PlugIn0, PlugIn1, PlugOut] {

    def caseZero[A](d : Derivative[_0, A], a : A) : Tree[_0, A] = Pt(a)
    def caseSucc[P <: Nat, A](d : Derivative[S[P], A], a : A) : Tree[S[P], A] = 
      d match {
        case Open(sh, ct) => Context.close(ct, Node(a, sh))
      }

  }

}


//============================================================================================
// Contexts
//

sealed abstract class Context[N <: Nat, +A] 
case class Empty[N <: Nat]() extends Context[N, Nothing] 
case class Then[N <: Nat, +A](a : A, d : Derivative[N, Tree[S[N], A]], ds : Context[S[N], A]) extends Context[S[N], A] 

object Context {

  //============================================================================================
  // CLOSE
  //

  def close[N <: Nat, A](c : Context[N, A], t : Tree[N, A]) : Tree[N, A] = ???

  type CloseIn0[N <: Nat, A] = Context[N, A]
  type CloseIn1[N <: Nat, A] = Tree[N, A]
  type CloseOut[N <: Nat, A] = Tree[N, A]

  object CloseRecursor extends NatRecursorT1P2[CloseIn0, CloseIn1, CloseOut] {

    def caseZero[A](c : Context[_0, A], t : Tree[_0, A]) : Tree[_0, A] = 
      c match {
        case Empty() => t
      }

    def caseSucc[P <: Nat, A](c : Context[S[P], A], t : Tree[S[P], A]) : Tree[S[P], A] = 
      c match {
        case Empty() => t
        case Then(a, d, ds) => caseSucc(ds, Node(a, Derivative.plug(d, t)))
      }

  }

}

//============================================================================================
// Zippers
//

sealed abstract class Zipper[N <: Nat, +A] {

  def focus : Tree[N, A]
  def context : Context[N, A]

}

case class FocusPoint[+A](pt : Tree[_0, A]) extends Zipper[_0, A] {

  def focus : Tree[_0, A] = pt
  def context : Context[_0, A] = Empty()

}

case class FocusList[+A](lst : Tree[_1, A], ctxt : Context[_1, A]) extends Zipper[_1, A] {

  def focus : Tree[_1, A] = lst
  def context : Context[_1, A] = ctxt

}

case class FocusBranch[N <: Nat, +A](tr : Tree[S[S[N]], A], ctxt : Context[S[S[N]], A]) extends Zipper[S[S[N]], A] {

  def focus : Tree[S[S[N]], A] = tr
  def context : Context[S[S[N]], A] = ctxt

}

object Zipper {

  //============================================================================================
  // VISIT
  //

  def visit[N <: Nat, A](d : Dir[N], z : Zipper[N, A]) : Option[Zipper[N, A]] =
    natOneRecT1P2(d.dim)(VisitRecursor)(d, z)

  type VisitIn0[N <: Nat, A] = Dir[N]
  type VisitIn1[N <: Nat, A] = Zipper[N, A]
  type VisitOut[N <: Nat, A] = Option[Zipper[N, A]]

  object VisitRecursor extends NatOneRecursorT1P2[VisitIn0, VisitIn1, VisitOut] {

    def caseZero[A](d : Dir[_0], z : Zipper[_0, A]) : Option[Zipper[_0, A]] = 
      Some(z)

    def caseOne[A](d : Dir[_1], z : Zipper[_1, A]) : Option[Zipper[_1, A]] =
      z match {
        case FocusList(Leaf(_), cntxt) => None
        case FocusList(Node(hd, Pt(tl)), cntxt) => Some(FocusList(tl, Then(hd, ZeroDeriv, cntxt)))
      }

    def caseDblSucc[P <: Nat, A](d : Dir[S[S[P]]], z : Zipper[S[S[P]], A]) : Option[Zipper[S[S[P]], A]] = 
      z match {
        case FocusBranch(Leaf(_), cntxt) => None
        case FocusBranch(Node(a, sh), cntxt) => 
          for {
            shZip <- Tree.seek(sh, d)
            resZip <- (
              shZip.focus match {
                case Leaf(_) => None
                case Node(t, tsh) => Some(FocusBranch(t, Then(a, Open(tsh, shZip.context), cntxt)))
              }
            )
          } yield resZip
      }

  }

  //============================================================================================
  // SEEK
  //

  def seek[N <: Nat, A](addr : Addr[N], zipper : Zipper[N, A]) : Option[Zipper[N, A]] =
    addr match {
      case Root() => Some(zipper)
      case Step(d, ds) =>
        for {
          zp <- seek(ds, zipper)
          zr <- visit(d, zp)
        } yield zr
    }

}
