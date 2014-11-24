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

}
