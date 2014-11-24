/**
  * Complex.scala - Complexes
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import scala.language.higherKinds

import scalaz.{Tree => _, Zipper => _, _}
import scalaz.std.option._

import Nats._
import Tree._
import Nesting._

object Complex {

  type Complex[N <: Nat, A] = Suite[Nesting, N, A]

  def toSigma[N <: Nat, A](cmplx : Complex[N, A]) : Sigma[Complex, A] = 
    new Sigma[Complex, A] {
      type Dim = N
      val dim = cmplx.dim
      val value = cmplx
    }

  //============================================================================================
  // MAP
  //

  def mapComplex[N <: Nat, A, B](cmplx : Complex[N, A])(f : A => B) : Complex[N, B] = 
    MapRecursor.execute(cmplx.dim)(cmplx, f)

  type MapIn0[M <: Nat, A, B] = Complex[M, A]
  type MapIn1[M <: Nat, A, B] = A => B
  type MapOut[M <: Nat, A, B] = Complex[M, B]

  object MapRecursor extends NatRecursorT2P2[MapIn0, MapIn1, MapOut] {

    def caseZero[A, B](cmplx : Complex[_0, A], f : A => B) : Complex[_0, B] = 
      cmplx match {
        case Base(nst) => Base(nst map f)
      }

    def caseSucc[P <: Nat, A, B](cmplx : Complex[S[P], A], f : A => B) : Complex[S[P], B] =
      cmplx match {
        case (tl >> hd) => mapComplex(tl)(f) >> (hd map f)
      }

  }

  //============================================================================================
  // TRAVERSE
  //

  def traverseComplex[N <: Nat, G[_], A, B](cmplx : Complex[N, A])(f : A => G[B])(implicit apG : Applicative[G]) : G[Complex[N, B]] = 
    TraverseRecursor.execute(cmplx.dim)(cmplx, f, apG)

  type TraverseIn0[N <: Nat, G[_], A, B] = Complex[N, A]
  type TraverseIn1[N <: Nat, G[_], A, B] = A => G[B]
  type TraverseIn2[N <: Nat, G[_], A, B] = Applicative[G]
  type TraverseOut[N <: Nat, G[_], A, B] = G[Complex[N, B]]

  object TraverseRecursor extends NatRecursorC1T2P3[TraverseIn0, TraverseIn1, TraverseIn2, TraverseOut] {

    def caseZero[G[_], A, B](cmplx : Complex[_0, A], f : A => G[B], apG : Applicative[G]) : G[Complex[_0, B]] = 
      cmplx match {
        case Base(nst) => 
          apG.ap(traverseNesting(nst)(f)(apG))(apG.pure((n : Nesting[_0, B]) => Base(n)))
      }

    def caseSucc[P <: Nat, G[_], A, B](cmplx : Complex[S[P], A], f : A => G[B], apG : Applicative[G]) : G[Complex[S[P], B]] = 
      cmplx match {
        case (tl >> hd) => {
          import apG._

          ap2(traverseComplex(tl)(f)(apG), traverseNesting(hd)(f)(apG))(
            pure((t : Complex[P, B], h : Nesting[S[P], B]) => t >> h)
          )
        }
      }

  }

  //============================================================================================
  // ZIP COMPLEX
  //

  def zipComplex[N <: Nat, A, B](cmplxA : Complex[N, A], cmplxB : Complex[N, B]) : Option[Complex[N, (A, B)]] = 
    ZipCompleteRecursor.execute(cmplxA.dim)(cmplxA, cmplxB)

  type ZipCompleteIn0[N <: Nat, A, B] = Complex[N, A]
  type ZipCompleteIn1[N <: Nat, A, B] = Complex[N, B]
  type ZipCompleteOut[N <: Nat, A, B] = Option[Complex[N, (A, B)]]

  object ZipCompleteRecursor extends NatRecursorT2P2[ZipCompleteIn0, ZipCompleteIn1, ZipCompleteOut] {

    def caseZero[A, B](cmplxA : Complex[_0, A], cmplxB : Complex[_0, B]) : Option[Complex[_0, (A, B)]] = 
      (cmplxA, cmplxB) match {
        case (Base(nA), Base(nB)) => 
          for {
            nAB <- nA matchWith nB
          } yield Base(nAB)
      }

    def caseSucc[P <: Nat, A, B](cmplxA : Complex[S[P], A], cmplxB : Complex[S[P], B]) : Option[Complex[S[P], (A, B)]] = 
      (cmplxA, cmplxB) match {
        case (tlA >> hdA, tlB >> hdB) => 
          for {
            tlAB <- zipComplex(tlA, tlB)
            hdAB <- hdA matchWith hdB
          } yield (tlAB >> hdAB)
      }

  }


  //============================================================================================
  // SOURCE ROUTINES
  //

  import ComplexZipper._

  def sourceAt[N <: Nat, A](cmplx : Complex[N, A], addr : Address[S[N]]) : Option[Complex[N, A]] = 
    for {
      c0 <- restrictAt(cmplx, addr)
      c1 <- contractAt(c0, Root()(addr.dim))
    } yield c1


  def restrictAt[N <: Nat, A](cmplx : Complex[N, A], addr : Address[S[N]]) : Option[Complex[N, A]] = {
    for {
      cz <- seek(addr, fromComplex(cmplx))
      cz0 <- restrictFocus(cz)
    } yield seal(cz0)
  }

  def contractAt[N <: Nat, A](cmplx : Complex[N, A], addr : Address[S[N]]) : Option[Complex[N, A]] = {
    for {
      cz <- seek(addr, fromComplex(cmplx))
      cz0 <- contractFocus(cz)
    } yield seal(cz0)
  }

  //============================================================================================
  // COMULTIPLY
  //

  def comultiply[N <: Nat, A](cmplx : Complex[N, A]) : Option[Complex[N, Sigma[Complex, A]]] =
    ComultiplyRecursor.execute(cmplx.dim)(cmplx)

  type ComultiplyIn[N <: Nat, A] = Complex[N, A]
  type ComultiplyOut[N <: Nat, A] = Option[Complex[N, Sigma[Complex, A]]]

  object ComultiplyRecursor extends NatRecursorT1P1[ComultiplyIn, ComultiplyOut] {

    def caseZero[A](cmplx : Complex[_0, A]) : Option[Complex[_0, Sigma[Complex, A]]] = 
      cmplx match {
        case Base(Obj(a)) => Some(Base(Obj(toSigma(Base(Obj(a))))))
        case Base(Box(a, Pt(nst))) => 
          for {
            Base(int) <- caseZero(Base(nst))
          } yield Base(Box(toSigma(Base(Obj(a))), Pt(int)))
      }

    def caseSucc[P <: Nat, A](cmplx : Complex[S[P], A]) : Option[Complex[S[P], Sigma[Complex, A]]] =
      cmplx match {
        case (tl >> hd) => {
          for {
            // You should have a separate method for traversing *with* the address.
            newHd <- hd.zipWithAddress traverse ({
              case (_, addr) => for { src <- sourceAt(tl >> hd, addr) } yield toSigma(src)
            })
            newTl <- comultiply(tl)
          } yield newTl >> newHd
        }
      }

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
