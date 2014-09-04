/**
  * Nat.scala - Type Level Natural Numbers
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import scala.language.higherKinds

import scalaz.Leibniz._

trait NatRec[Type] {

  type OnZero <: Type
  type OnSucc[P <: Nat, T <: Type] <: Type

}

trait NatConsRec[Type] {

  type OnZero[+A] <: Type
  type OnSucc[P <: Nat, T[+_] <: Type, +A] <: Type

}

sealed trait Nat { self => 

  type Self <: Nat

  val self : Self = this.asInstanceOf[Self]

  type Rec[Type, R <: NatRec[Type]] <: Type
  type ConsRec[Type, C <: NatConsRec[Type], +A] <: Type

}

case object Z extends Nat {

  type Self = Z.type

  type Rec[Type, R <: NatRec[Type]] = R#OnZero
  type ConsRec[Type, C <: NatConsRec[Type], +A] = C#OnZero[A]

}


case class S[P <: Nat](p : P) extends Nat {

  type Self = S[P]

  type Rec[Type, R <: NatRec[Type]] = R#OnSucc[P, P#Rec[Type, R]]
  type ConsRec[Type, C <: NatConsRec[Type], +A] = 
    C#OnSucc[P, ({ type L[+X] = P#ConsRec[Type, C, X] })#L, A]

}

trait Nats {

  type __0 = Z.type
  type __1 = S[__0]
  type __2 = S[__1]
  type __3 = S[__2]
  type __4 = S[__3]
  type __5 = S[__4]
  type __6 = S[__5]
  type __7 = S[__6]
  type __8 = S[__7]
  type __9 = S[__8]

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
