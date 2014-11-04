/**
  * Complex.scala - Complexes
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import scalaz.{Tree => _, Zipper => _, _}
import scalaz.std.option._

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

  def head[N <: Nat, A](cmplx : Complex[N, A]) : Nesting[N, A] = 
    HeadRecursor.execute(cmplx.dim)(cmplx)

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

  //============================================================================================
  // SOURCE ROUTINES
  //

  def sourceAt[N <: Nat, A](cmplx : Complex[N, A], addr : Address[S[N]]) : Option[Complex[N, A]] = 
    for {
      c0 <- restrictAt(cmplx, addr)
      c1 <- contractAt(c0, Root()(addr.dim))
    } yield c1


  def restrictAt[N <: Nat, A](cmplx : Complex[N, A], addr : Address[S[N]]) : Option[Complex[N, A]] = {
    import ComplexZipper._

    for {
      cz <- seek(addr, fromComplex(cmplx))
      cz0 <- restrictFocus(cz)
    } yield seal(cz0)
  }

  def contractAt[N <: Nat, A](cmplx : Complex[N, A], addr : Address[S[N]]) : Option[Complex[N, A]] = {
    import ComplexZipper._

    for {
      cz <- seek(addr, fromComplex(cmplx))
      cz0 <- contractFocus(cz)
    } yield seal(cz0)
  }

  //============================================================================================
  // SOURCE CALCULATION MONAD
  //

  type SourceM[N <: Nat, A, R] = StateT[Option, Complex[N, A], R]

  def liftS[N <: Nat, A, R](opt : Option[R]) : SourceM[N, A, R] =
    StateT((cmplx : Complex[N, A]) => opt map (r => (cmplx, r)))

  def exciseLocal[N <: Nat, A](addr : Address[S[N]], tr : Tree[S[N], A]) : SourceM[N, A, Unit] = {

    type SrcM[R] = SourceM[N, A, R]
    type SrcS[S, R] = StateT[Option, S, R]

    val MS = MonadState[SrcS, Complex[N, A]]
    import MS._

    tr match {
      case Leaf(_) =>
        for {
          complex <- get
          contractResult <- liftS(contractAt(complex, addr))
          _ <- put(contractResult)
        } yield ()
      case Node(a, sh) =>
        for {  // A bit ugly ....
          _ <- Tree.traverse[N, SrcM, (Tree[S[N], A], Address[N]), Unit](zipWithAddress(sh))({ 
            case (t, d) => exciseLocal(Step(d, addr), t) 
          })(implicitly[Applicative[SrcM]])
        } yield ()
    }
  }


}
