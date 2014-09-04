/**
  * Nat.scala - Type Level Natural Numbers
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import scala.language.higherKinds

import scalaz.Leibniz._

trait NatRec0[Type] {

  type OnZero <: Type
  type OnSucc[P <: Nat, T <: Type] <: Type

}

trait NatRec1[Type] {

  type OnZero[+A] <: Type
  type OnSucc[P <: Nat, T[+_] <: Type, +A] <: Type

}

sealed trait Nat { self => 

  type Self <: Nat

  val self : Self = this.asInstanceOf[Self]

  type Rec0[Type, R <: NatRec0[Type]] <: Type
  type Rec1[Type, C <: NatRec1[Type], +A] <: Type

}

case object Z extends Nat {

  type Self = Z.type

  type Rec0[Type, R <: NatRec0[Type]] = R#OnZero
  type Rec1[Type, C <: NatRec1[Type], +A] = C#OnZero[A]

}


case class S[P <: Nat](p : P) extends Nat {

  type Self = S[P]

  type Rec0[Type, R <: NatRec0[Type]] = 
    R#OnSucc[P, P#Rec0[Type, R]]

  type Rec1[Type, C <: NatRec1[Type], +A] = 
    C#OnSucc[P, ({ type L[+X] = P#Rec1[Type, C, X] })#L, A]

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

  trait ZeroMatch[N <: Nat] {

    implicit def isZero : N === Z.type

  }

  trait SuccMatch[N <: Nat] {

    type P <: Nat

    val p : P

    implicit def isSucc : N === S[P]

  }

  object IsZero {

    def unapply[N <: Nat](n : N) : Option[ZeroMatch[N]] =
      n match {
        case Z => Some(
          new ZeroMatch[N] { 
            def isZero : N === Z.type = force[Nothing, Any, N, Z.type] 
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

            def isSucc : N === S[P] = force[Nothing, Any, N, S[P]]

          }
        )
        case _ => None
      }
  }

}

object Nats extends Nats
