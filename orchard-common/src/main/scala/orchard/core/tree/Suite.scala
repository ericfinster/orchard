/**
  * Suite.scala - A Sequence of an indexed type
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import scala.language.higherKinds
import scala.language.implicitConversions

import Nats._

sealed abstract class Suite[F[_ <: Nat, _], N <: Nat, A] {

  def dim : N
  def head : F[N, A]

  def >>(hd : F[S[N], A]) : Suite[F, S[N], A] = new >>(this, hd)

}

case class Base[F[_ <: Nat, _], A](el : F[_0, A]) extends Suite[F, _0, A] { def head = el ; def dim = Z }
case class >>[F[_ <: Nat, _], N <: Nat, A](tl : Suite[F, N, A], hd : F[S[N], A]) extends Suite[F, S[N], A] { def head = hd ; def dim = S(tl.dim) }

object Suite {

  implicit def toBase[F[_ <: Nat, _], A](el : F[_0, A]) : Suite[F, _0, A] = Base(el)

  def tailOf[F[_ <: Nat, _], N <: Nat, A](suite : Suite[F, S[N], A]) : Suite[F, N, A] = 
    suite match {
      case (tl >> _) => tl
    }

  def truncate[F[_ <: Nat, _], M <: Nat, N <: Nat, A](suite : Suite[F, N, A])(implicit lte : Lte[M, N]) : Suite[F, M, A] = {

    type TruncateIn[P <: Nat, Q <: Nat] = Suite[F, Q, A]
    type TruncateOut[P <: Nat, Q <: Nat] = Suite[F, P, A]

    object TruncateRecursor extends LteRecursor[TruncateIn, TruncateOut] {

      def caseZeroLte[P <: Nat](s : Suite[F, P, A]) : Suite[F, _0, A] = ???
      def caseSuccLte[P <: Nat, Q <: Nat](s : Suite[F, S[Q], A], lte : Lte[P, Q]) : Suite[F, S[P], A] = ???

    }

    //TruncateRecursor.execute(suite)(gte)

    ???
  }


  // trait GteRecursor[F[_ <: Nat, _ <: Nat], G[_ <: Nat, _ <: Nat]] {

  //   def caseRefl[N <: Nat](fnn : F[N, N], n : N) : G[N, N]
  //   def caseTrans[N <: Nat, M <: Nat](fsnm : F[S[N], M], gte : Gte[N, M]) : G[S[N], M]

  //   def execute[N <: Nat, M <: Nat](fnm : F[N, M])(gte : Gte[N, M]) : G[N, M] = 
  //     gte match {
  //       case Refl(n) => {
  //         caseRefl(fnm.asInstanceOf[F[N, N]], n).asInstanceOf[G[N, M]]
  //       }
  //       case t @ Trans(g) => {
  //         caseTrans[t.P, M](fnm.asInstanceOf[F[S[t.P], M]], g).asInstanceOf[G[N, M]]
  //       }
  //     }

  // }

}
