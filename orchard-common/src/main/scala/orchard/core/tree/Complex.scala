/**
  * Complex.scala - Complexes
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import Nats._
import Tree._

// A useful generalization of this might be to allow the type
// "A" to depend on the dimension.  Doing this would allow for
// a well typed comultiplication operation ....

sealed abstract class Complex[N <: Nat, +A] { def dim : N }
case class Base[+A](objNst : Nesting[_0, A]) extends Complex[_0, A] { def dim = Z }
case class Append[N <: Nat, +A](tl : Complex[N, A], nst : Nesting[S[N], A]) extends Complex[S[N], A] { def dim = nst.dim }

object Complex {

  //============================================================================================
  // HEAD
  //

  def head[N <: Nat, A](cmplx : Complex[N, A]) : Nesting[N, A] = ???

  type HeadIn[N <: Nat, A] = Complex[N, A]
  type HeadOut[N <: Nat, A] = Nesting[N, A]

  object HeadRecursor extends NatRecursorT1P1[HeadIn, HeadOut] {

    def caseZero[A](cmplx : Complex[_0, A]) : Nesting[_0, A] = 
      cmplx match {
        case Base(objNst) => objNst
      }

    def caseSucc[P <: Nat, A](cmplx : Complex[S[P], A]) : Nesting[S[P], A] =
      cmplx match {
        case Append(_, nst) => nst
      }

  }

}
