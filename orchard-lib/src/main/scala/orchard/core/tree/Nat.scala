/**
  * Nat.scala - Type Level Natural Numbers
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import scala.language.higherKinds

import scalaz.Leibniz
import scalaz.Leibniz._

trait NatRec0[Type] {

  type OnZero <: Type
  type OnSucc[P <: Nat, T <: Type] <: Type

}

trait NatRec1[Type] {

  type OnZero[+A] <: Type
  type OnSucc[P <: Nat, T[+_] <: Type, +A] <: Type

}

trait NatRecPair1[Type] {

  type OnZeroFst[+A] <: Type
  type OnZeroSnd[+A] <: Type

  type OnSuccFst[P <: Nat, F[+_] <: Type, G[+_] <: Type, +A] <: Type
  type OnSuccSnd[P <: Nat, F[+_] <: Type, G[+_] <: Type, +A] <: Type

}

sealed trait Nat { self => 

  type Self <: Nat

  val self : Self = this.asInstanceOf[Self]

  type Rec0[Type, R <: NatRec0[Type]] <: Type
  type Rec1[Type, C <: NatRec1[Type], +A] <: Type

  type RecPairFst[Type, R <: NatRecPair1[Type], +A] <: Type
  type RecPairSnd[Type, R <: NatRecPair1[Type], +A] <: Type

}

case object Z extends Nat {

  type Self = Z.type

  type Rec0[Type, R <: NatRec0[Type]] = R#OnZero
  type Rec1[Type, C <: NatRec1[Type], +A] = C#OnZero[A]

  type RecPairFst[Type, R <: NatRecPair1[Type], +A] = R#OnZeroFst[A]
  type RecPairSnd[Type, R <: NatRecPair1[Type], +A] = R#OnZeroSnd[A]

}


case class S[P <: Nat](val pred : P) extends Nat {

  type Self = S[P]

  type Rec0[Type, R <: NatRec0[Type]] = 
    R#OnSucc[P, P#Rec0[Type, R]]

  type Rec1[Type, C <: NatRec1[Type], +A] = 
    C#OnSucc[P, ({ type L[+X] = P#Rec1[Type, C, X] })#L, A]

  type RecPairFst[Type, R <: NatRecPair1[Type], +A] = 
    R#OnSuccFst[P, 
      ({ type L[+X] = P#RecPairFst[Type, R, X] })#L,
      ({ type L[+X] = P#RecPairSnd[Type, R, X] })#L,
      A]

  type RecPairSnd[Type, R <: NatRecPair1[Type], +A] =
    R#OnSuccSnd[P, 
      ({ type L[+X] = P#RecPairFst[Type, R, X] })#L,
      ({ type L[+X] = P#RecPairSnd[Type, R, X] })#L,
      A]

}

trait Nats {

  type _0 = Z.type
  type _1 = S[_0]
  type _2 = S[_1]
  type _3 = S[_2]
  type _4 = S[_3]
  type _5 = S[_4]
  type _6 = S[_5]
  type _7 = S[_6]
  type _8 = S[_7]
  type _9 = S[_8]

  implicit def zeroNat : Z.type = Z
  implicit def succNat[P <: Nat](implicit p : P) : S[P] = S(p)

  def fromInt(i : Int) : Nat = 
    if (i <= 0) Z else S(fromInt(i - 1))

  def toInt[N <: Nat](n : N) : Int = 
    n match {
      case Z => 0
      case S(p) => toInt(p) + 1
    }

  trait HasPred[N <: Nat] {
    type P <: Nat
    def leibniz : Leibniz[Nothing, Nat, S[P], N]
  }

  implicit def succHasPred[N <: Nat] : HasPred[S[N]] = 
    new HasPred[S[N]] {
      type P = N
      def leibniz : Leibniz[Nothing, Nat, S[P], S[P]] = refl[S[P]]
    }

  trait ZeroMatch[N <: Nat] {

    implicit def zeroCoh : N === _0
    implicit def zeroCoe : _0 === N

  }

  trait OneMatch[N <: Nat] {

    implicit def oneCoh : N === _1
    implicit def oneCoe : _1 === N

  }

  trait SuccMatch[N <: Nat] {

    type P <: Nat

    implicit val p : P

    implicit def succCoh : N === S[P]
    implicit def succCoe : S[P] === N

  }

  trait DblSuccMatch[N <: Nat] {

    type PP <: Nat
    type P = S[PP]

    implicit val pp : PP
    implicit val p = S(pp)

    implicit def dblSuccCoh : N === S[S[PP]]
    implicit def dblSuccCoe : S[S[PP]] === N

  }

  object IsZero {

    def unapply[N <: Nat](n : N) : Option[ZeroMatch[N]] =
      n match {
        case Z => Some(
          new ZeroMatch[N] { 
            implicit def zeroCoh : N === _0 = force[Nothing, Any, N, _0]
            implicit def zeroCoe : _0 === N = force[Nothing, Any, _0, N]
          }
        )
        case _ => None
      }

  }

  object IsOne {

    def unapply[N <: Nat](n : N) : Option[OneMatch[N]] = 
      n match {
        case S(S(Z)) => Some(
          new OneMatch[N] {
            implicit def oneCoh : N === _1 = force[Nothing, Any, N, _1]
            implicit def oneCoe : _1 === N = force[Nothing, Any, _1, N]
          }
        )
        case _ => None
      }

  }

  object IsSucc {

    def unapply[N <: Nat](n : N) : Option[SuccMatch[N]] = 
      n match {
        case S(pr) => Some(
          new SuccMatch[N] {

            type P = pr.Self
            val p = pr.self

            def succCoh : N === S[P] = force[Nothing, Any, N, S[P]]
            def succCoe : S[P] === N = force[Nothing, Any, S[P], N]
            
          }
        )
        case _ => None
      }

  }

  object IsDblSucc {

    def unapply[N <: Nat](n : N) : Option[DblSuccMatch[N]] = 
      n match {
        case S(S(ppr)) => Some(
          new DblSuccMatch[N] {

            type PP = ppr.Self
            val pp = ppr.self

            implicit def dblSuccCoh : N === S[S[PP]] = force[Nothing, Any, N, S[S[PP]]]
            implicit def dblSuccCoe : S[S[PP]] === N = force[Nothing, Any, S[S[PP]], N]

          }
        )
        case _ => None
      }

  }

}

object Nats extends Nats
