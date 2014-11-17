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

object Complex {

  type Complex[N <: Nat, A] = Suite[Nesting, N, A]

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

  // //============================================================================================
  // // FACE AT
  // //

  // def faceAt[N <: Nat, M <: Nat, A](cmplx : Complex[N, A], addr : Address[S[M]])(implicit gte : Gte[N, M]) 
  //     : Option[Complex[M, A]] = {
  //   sourceAt(Suite.truncate(cmplx), addr)
  // }

  // def faceAtLocation[N <: Nat, A](cmplx : Complex[N, A], faceAddr : FaceAddress[N]) : Option[Complex[faceAddr.Dim, A]] = 
  //   faceAt(cmplx, faceAddr.address)(faceAddr.gte)

  // //============================================================================================
  // // COMULTIPLICATION ROUTINES
  // //

  // def addrComult[N <: Nat, A](cmplx : Complex[N, A]) : Complex[N, (A, FaceAddress[N])] = ???

  // def addrComultLocal[N <: Nat, M <: Nat, A](cmplx : Complex[M, A], gte : Gte[N, M]) : Complex[M, (A, FaceAddress[N])] = {

  //   type ComultIn0[P <: Nat] = Complex[P, A]
  //   type ComultIn1[P <: Nat] = Gte[N, P]
  //   type ComultOut[P <: Nat] = Complex[P, (A, FaceAddress[N])]

  //   object ComultRecursor extends NatRecursorT0P2[ComultIn0, ComultIn1, ComultOut] {

  //     def caseZero(cmplx : Complex[_0, A], gte : Gte[N, _0]) : Complex[_0, (A, FaceAddress[N])] = ???
  //     def caseSucc[P <: Nat](cmplx : Complex[S[P], A], gte : Gte[N, S[P]]) : Complex[S[P], (A, FaceAddress[N])] = ???

  //   }

  //   ???

  // }


  // def unwind[N <: Nat, M <: Nat](gte : Gte[N, S[M]]) : Gte[N, M] = ???

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
