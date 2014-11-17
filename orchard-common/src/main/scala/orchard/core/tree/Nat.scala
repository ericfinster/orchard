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

//============================================================================================
// TYPE LEVEL NATURALS
//

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


case class S[P <: Nat](val pred : P) extends Nat { self =>

  type Self = S[P]

  type Rec0[Type, R <: NatRec0[Type]] = 
    R#OnSucc[P, P#Rec0[Type, R]]

  type Rec1[Type, C <: NatRec1[Type], +A] = 
    C#OnSucc[P, ({ type L[+X] = P#Rec1[Type, C, X] })#L, A]

}

//============================================================================================
// TYPE RECURSORS
//

trait NatRec0[Type] {

  type OnZero <: Type
  type OnSucc[P <: Nat, T <: Type] <: Type

}

trait NatRec1[Type] {

  type OnZero[+A] <: Type
  type OnSucc[P <: Nat, T[+_] <: Type, +A] <: Type

}

//============================================================================================
// VALUE RECURSORS
//

trait NatRecursors { self : Nats => 

  trait NatRecursorT0P1[F[_ <: Nat], G[_ <: Nat]] {

    def caseZero(fz : F[_0]) : G[_0]
    def caseSucc[P <: Nat](fs : F[S[P]]) : G[S[P]]

    def execute[N <: Nat](n : N)(fn : F[N]) : G[N] = 
      n match {
        case IsZero(zm) => {
          import zm._
          val fz : F[_0] = zeroCoh.subst[F](fn)
          val gz : G[_0] = caseZero(fz)
          zeroCoe.subst[G](gz)
        }
        case IsSucc(sm) => {
          import sm._
          val fs : F[S[P]] = succCoh.subst[F](fn)
          val gs : G[S[P]] = caseSucc[P](fs)
          succCoe.subst[G](gs)
        }
      }

  }

  trait NatRecursorT0P2[F[_ <: Nat], G[_ <: Nat], H[_ <: Nat]] {

    def caseZero(fz : F[_0], gz : G[_0]) : H[_0]
    def caseSucc[P <: Nat](fs : F[S[P]], gs : G[S[P]]) : H[S[P]]

    def execute[N <: Nat](n : N)(fn : F[N], gn : G[N]) : H[N] = 
      n match {
        case IsZero(zm) => {
          import zm._
          val fz : F[_0] = zeroCoh.subst[F](fn)
          val gz : G[_0] = zeroCoh.subst[G](gn)
          val hz : H[_0] = caseZero(fz, gz)
          zeroCoe.subst[H](hz)
        }
        case IsSucc(sm) => {
          import sm._
          val fs : F[S[P]] = succCoh.subst[F](fn)
          val gs : G[S[P]] = succCoh.subst[G](gn)
          val hs : H[S[P]] = caseSucc[P](fs, gs)
          succCoe.subst[H](hs)
        }
      }

  }


  trait NatRecursorT1P1[F[_ <: Nat, _], G[_ <: Nat, _]] {

    def caseZero[A](fz : F[_0, A]) : G[_0, A]
    def caseSucc[P <: Nat, A](fs : F[S[P], A]) : G[S[P], A]

    def execute[N <: Nat, A](n : N)(fn : F[N, A]) : G[N, A] = 
      n match {
        case IsZero(zm) => {
          import zm._
          val fz : F[_0, A] = zeroCoh.subst[({ type L[M <: Nat] = F[M, A] })#L](fn)
          val gz : G[_0, A] = caseZero(fz)
          zeroCoe.subst[({ type L[M <: Nat] = G[M, A] })#L](gz)
        }
        case IsSucc(sm) => {
          import sm._
          val fs : F[S[P], A] = succCoh.subst[({ type L[M <: Nat] = F[M, A] })#L](fn)
          val gs : G[S[P], A] = caseSucc[P, A](fs)
          succCoe.subst[({ type L[M <: Nat] = G[M, A] })#L](gs)
        }
      }

  }

  trait NatRecursorT1P2[F[_ <: Nat, _], G[_ <: Nat, _], H[_ <: Nat, _]] {

    def caseZero[A](fz : F[_0, A], gz : G[_0, A]) : H[_0, A]
    def caseSucc[P <: Nat, A](fs : F[S[P], A], gs : G[S[P], A]) : H[S[P], A]

    def execute[N <: Nat, A](n : N)(fn : F[N, A], gn : G[N, A]) : H[N, A] = 
      n match {
        case IsZero(zm) => {
          import zm._
          val fz : F[_0, A] = zeroCoh.subst[({ type L[M <: Nat] = F[M, A] })#L](fn)
          val gz : G[_0, A] = zeroCoh.subst[({ type L[M <: Nat] = G[M, A] })#L](gn)
          val hz : H[_0, A] = caseZero(fz, gz)
          zeroCoe.subst[({ type L[M <: Nat] = H[M, A] })#L](hz)
        }
        case IsSucc(sm) => {
          import sm._
          val fs : F[S[P], A] = succCoh.subst[({ type L[M <: Nat] = F[M, A] })#L](fn)
          val gs : G[S[P], A] = succCoh.subst[({ type L[M <: Nat] = G[M, A] })#L](gn)
          val hs : H[S[P], A] = caseSucc[P, A](fs, gs)
          succCoe.subst[({ type L[M <: Nat] = H[M, A] })#L](hs)
        }
      }

  }

  trait NatRecursorT2P1[F[_ <: Nat, _, _], G[_ <: Nat, _, _]] {

    def caseZero[A, B](fz : F[_0, A, B]) : G[_0, A, B]
    def caseSucc[P <: Nat, A, B](fs : F[S[P], A, B]) : G[S[P], A, B]

    def execute[N <: Nat, A, B](n : N)(fn : F[N, A, B]) : G[N, A, B] = 
      n match {
        case IsZero(zm) => {
          import zm._
          val fz : F[_0, A, B] = zeroCoh.subst[({ type L[M <: Nat] = F[M, A, B] })#L](fn)
          val gz : G[_0, A, B] = caseZero(fz)
          zeroCoe.subst[({ type L[M <: Nat] = G[M, A, B] })#L](gz)
        }
        case IsSucc(sm) => {
          import sm._
          val fs : F[S[P], A, B] = succCoh.subst[({ type L[M <: Nat] = F[M, A, B] })#L](fn)
          val gs : G[S[P], A, B] = caseSucc[P, A, B](fs)
          succCoe.subst[({ type L[M <: Nat] = G[M, A, B] })#L](gs)
        }
      }

  }

  trait NatRecursorT2P2[F[_ <: Nat, _, _], G[_ <: Nat, _, _], H[_ <: Nat, _, _]] {

    def caseZero[A, B](fz : F[_0, A, B], gz : G[_0, A, B]) : H[_0, A, B]
    def caseSucc[P <: Nat, A, B](fs : F[S[P], A, B], gs : G[S[P], A, B]) : H[S[P], A, B]

    def execute[N <: Nat, A, B](n : N)(fn : F[N, A, B], gn : G[N, A, B]) : H[N, A, B] = 
      n match {
        case IsZero(zm) => {
          import zm._
          val fz : F[_0, A, B] = zeroCoh.subst[({ type L[M <: Nat] = F[M, A, B] })#L](fn)
          val gz : G[_0, A, B] = zeroCoh.subst[({ type L[M <: Nat] = G[M, A, B] })#L](gn)
          val hz : H[_0, A, B] = caseZero(fz, gz)
          zeroCoe.subst[({ type L[M <: Nat] = H[M, A, B] })#L](hz)
        }
        case IsSucc(sm) => {
          import sm._
          val fs : F[S[P], A, B] = succCoh.subst[({ type L[M <: Nat] = F[M, A, B] })#L](fn)
          val gs : G[S[P], A, B] = succCoh.subst[({ type L[M <: Nat] = G[M, A, B] })#L](gn)
          val hs : H[S[P], A, B] = caseSucc[P, A, B](fs, gs)
          succCoe.subst[({ type L[M <: Nat] = H[M, A, B] })#L](hs)
        }
      }

  }

  trait NatRecursorC1T2P1[F[_ <: Nat, _[_], _, _], G[_ <: Nat, _[_], _, _]] {

    def caseZero[T[_], A, B](fz : F[_0, T, A, B]) : G[_0, T, A, B]
    def caseSucc[P <: Nat, T[_], A, B](fs : F[S[P], T, A, B]) : G[S[P], T, A, B]

    def execute[N <: Nat, T[_], A, B](n : N)(fn : F[N, T, A, B]) : G[N, T, A, B] = 
      n match {
        case IsZero(zm) => {
          import zm._
          val fz : F[_0, T, A, B] = zeroCoh.subst[({ type L[M <: Nat] = F[M, T, A, B] })#L](fn)
          val gz : G[_0, T, A, B] = caseZero(fz)
          zeroCoe.subst[({ type L[M <: Nat] = G[M, T, A, B] })#L](gz)
        }
        case IsSucc(sm) => {
          import sm._
          val fs : F[S[P], T, A, B] = succCoh.subst[({ type L[M <: Nat] = F[M, T, A, B] })#L](fn)
          val gs : G[S[P], T, A, B] = caseSucc[P, T, A, B](fs)
          succCoe.subst[({ type L[M <: Nat] = G[M, T, A, B] })#L](gs)
        }
      }

  }

  trait NatRecursorC1T2P3[F[_ <: Nat, _[_], _, _], G[_ <: Nat, _[_], _, _], H[_ <: Nat, _[_], _, _], K[_ <: Nat, _[_], _, _]] {

    def caseZero[T[_], A, B](fz : F[_0, T, A, B], gz : G[_0, T, A, B], hz : H[_0, T, A, B]) : K[_0, T, A, B]
    def caseSucc[P <: Nat, T[_], A, B](fs : F[S[P], T, A, B], gs : G[S[P], T, A, B], hs : H[S[P], T, A, B]) : K[S[P], T, A, B]

    def execute[N <: Nat, T[_], A, B](n : N)(fn : F[N, T, A, B], gn : G[N, T, A, B], hn : H[N, T, A, B]) : K[N, T, A, B] =
      n match {
        case IsZero(zm) => {
          import zm._
          val fz : F[_0, T, A, B] = zeroCoh.subst[({ type L[M <: Nat] = F[M, T, A, B] })#L](fn)
          val gz : G[_0, T, A, B] = zeroCoh.subst[({ type L[M <: Nat] = G[M, T, A, B] })#L](gn)
          val hz : H[_0, T, A, B] = zeroCoh.subst[({ type L[M <: Nat] = H[M, T, A, B] })#L](hn)
          val kz : K[_0, T, A, B] = caseZero(fz, gz, hz)
          zeroCoe.subst[({ type L[M <: Nat] = K[M, T, A, B] })#L](kz)
        }
        case IsSucc(sm) => {
          import sm._
          val fs : F[S[P], T, A, B] = succCoh.subst[({ type L[M <: Nat] = F[M, T, A, B] })#L](fn)
          val gs : G[S[P], T, A, B] = succCoh.subst[({ type L[M <: Nat] = G[M, T, A, B] })#L](gn)
          val hs : H[S[P], T, A, B] = succCoh.subst[({ type L[M <: Nat] = H[M, T, A, B] })#L](hn)
          val ks : K[S[P], T, A, B] = caseSucc[P, T, A, B](fs, gs, hs)
          succCoe.subst[({ type L[M <: Nat] = K[M, T, A, B] })#L](ks)
        }
      }

  }

  trait NatOneRecursorT1P2[F[_ <: Nat, _], G[_ <: Nat, _], H[_ <: Nat, _]] {

    def caseZero[A](fz : F[_0, A], gz : G[_0, A]) : H[_0, A]
    def caseOne[A](fo : F[_1, A], go : G[_1, A]) : H[_1, A]
    def caseDblSucc[P <: Nat, A](fs : F[S[S[P]], A], gs : G[S[S[P]], A]) : H[S[S[P]], A]

    def execute[N <: Nat, A](n : N)(fn : F[N, A], gn : G[N, A]) : H[N, A] = 
      n match {
        case IsZero(zm) => {
          import zm._
          val fz : F[_0, A] = zeroCoh.subst[({ type L[M <: Nat] = F[M, A] })#L](fn)
          val gz : G[_0, A] = zeroCoh.subst[({ type L[M <: Nat] = G[M, A] })#L](gn)
          val hz : H[_0, A] = caseZero(fz, gz)
          zeroCoe.subst[({ type L[M <: Nat] = H[M, A] })#L](hz)
        }
        case IsOne(om) => {
          import om._
          val fo : F[_1, A] = oneCoh.subst[({ type L[M <: Nat] = F[M, A] })#L](fn)
          val go : G[_1, A] = oneCoh.subst[({ type L[M <: Nat] = G[M, A] })#L](gn)
          val ho : H[_1, A] = caseOne(fo, go)
          oneCoe.subst[({ type L[M <: Nat] = H[M, A] })#L](ho)
        }
        case IsDblSucc(dm) => {
          import dm._
          val fds : F[S[S[PP]], A] = dblSuccCoh.subst[({ type L[M <: Nat] = F[M, A] })#L](fn)
          val gds : G[S[S[PP]], A] = dblSuccCoh.subst[({ type L[M <: Nat] = G[M, A] })#L](gn)
          val hds : H[S[S[PP]], A] = caseDblSucc[PP, A](fds, gds)
          dblSuccCoe.subst[({ type L[M <: Nat] = H[M, A] })#L](hds)
        }
      }

  }

}

trait Nats extends NatRecursors {

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

  def natFromInt(i : Int) : Nat = 
    if (i <= 0) Z else S(natFromInt(i - 1))

  def natToInt[N <: Nat](n : N) : Int = 
    n match {
      case Z => 0
      case S(p) => natToInt(p) + 1
    }

  implicit def zeroNat : Z.type = Z
  implicit def succNat[P <: Nat](implicit p : P) : S[P] = S(p)

  trait IsZero[N <: Nat] {
    def leibniz : Leibniz[Nothing, Nat, _0, N]
  }

  trait IsSucc[N <: Nat] {
    type P <: Nat
    def leibniz : Leibniz[Nothing, Nat, S[P], N]
  }

  implicit def zeroIsZero : IsZero[_0] =
    new IsZero[_0] {
      def leibniz : Leibniz[Nothing, Nat, _0, _0] = refl[_0]
    }

  implicit def succIsSucc[N <: Nat] : IsSucc[S[N]] = 
    new IsSucc[S[N]] {
      type P = N
      def leibniz : Leibniz[Nothing, Nat, S[P], S[P]] = refl[S[P]]
    }

  sealed trait Lte[M <: Nat, N <: Nat]
  case class ZeroLte[N <: Nat]() extends Lte[_0, N] 
  case class SuccLte[M <: Nat, N <: Nat](lteMN : Lte[M, N]) extends Lte[S[M], S[N]] { type P = M ; type Q = N }

  trait LteRecursor[F[_ <: Nat, _ <: Nat], G[_ <: Nat, _ <: Nat]] {

    def caseZeroLte[N <: Nat](fzs : F[_0, N]) : G[_0, N]
    def caseSuccLte[M <: Nat, N <: Nat](fss : F[S[M], S[N]], lte : Lte[M, N]) : G[S[M], S[N]]

    def execute[M <: Nat, N <: Nat](ev : Lte[M, N])(fnm : F[M, N]) : G[M, N] = 
      ev match {
        case zs @ ZeroLte() => caseZeroLte(fnm.asInstanceOf[F[_0, N]]).asInstanceOf[G[M, N]]
        case ss @ SuccLte(lte) => caseSuccLte(fnm.asInstanceOf[F[S[ss.P], S[ss.Q]]], lte).asInstanceOf[G[M, N]]
      }

  }

  // def lteLemma[M <: Nat, N <: Nat](lte : Lte[S[M], N]) : Lte[M, N] = 
  //   lte match {
  //     case ZeroRefl => ???
  //     case ZeroSucc() => ???
  //     case SuccSucc(_) => ???
  //   }

  // object Lte {

  //   implicit def apply[M <: Nat, N <: Nat](implicit lte : Lte[M, N]) : Lte[M, N] = lte

  //   type <=[M <: Nat, N <: Nat] = Lte[M, N]

  //   implicit def lteReflZero = new <=[_0, _0] { }
  //   implicit def lteSuccZero[M <: Nat] = new <=[_0, S[M]] { }
  //   implicit def lteSuccSucc[M <: Nat, N <: Nat](implicit lte : M <= N) = new <=[S[M], S[N]] { }

  //   def lteLemma[M <: Nat, N <: Nat](lte : S[M] <= N) : M <= N = ???

  // }

  trait ZeroMatch[N <: Nat] {

    implicit def zeroCoh : Leibniz[Nothing, Nat, N, _0]
    implicit def zeroCoe : Leibniz[Nothing, Nat, _0, N]

  }

  trait OneMatch[N <: Nat] {

    implicit def oneCoh : Leibniz[Nothing, Nat, N, _1]
    implicit def oneCoe : Leibniz[Nothing, Nat, _1, N]

  }

  trait SuccMatch[N <: Nat] {

    type P <: Nat

    implicit val p : P

    implicit def succCoh : Leibniz[Nothing, Nat, N, S[P]]
    implicit def succCoe : Leibniz[Nothing, Nat, S[P], N]

  }

  trait DblSuccMatch[N <: Nat] {

    type PP <: Nat
    type P = S[PP]

    implicit val pp : PP
    implicit val p = S(pp)

    implicit def dblSuccCoh : Leibniz[Nothing, Nat, N, S[S[PP]]]
    implicit def dblSuccCoe : Leibniz[Nothing, Nat, S[S[PP]], N]

  }

  object IsZero {

    def unapply[N <: Nat](n : N) : Option[ZeroMatch[N]] =
      n match {
        case Z => Some(
          new ZeroMatch[N] { 
            implicit def zeroCoh : Leibniz[Nothing, Nat, N, _0] = force[Nothing, Nat, N, _0]
            implicit def zeroCoe : Leibniz[Nothing, Nat, _0, N] = force[Nothing, Nat, _0, N]
          }
        )
        case _ => None
      }

  }

  object IsOne {

    def unapply[N <: Nat](n : N) : Option[OneMatch[N]] = 
      n match {
        case S(Z) => Some(
          new OneMatch[N] {
            implicit def oneCoh : Leibniz[Nothing, Nat, N, _1] = force[Nothing, Nat, N, _1]
            implicit def oneCoe : Leibniz[Nothing, Nat, _1, N] = force[Nothing, Nat, _1, N]
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

            def succCoh : Leibniz[Nothing, Nat, N, S[P]] = force[Nothing, Nat, N, S[P]]
            def succCoe : Leibniz[Nothing, Nat, S[P], N] = force[Nothing, Nat, S[P], N]
            
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

            implicit def dblSuccCoh : Leibniz[Nothing, Nat, N, S[S[PP]]] = force[Nothing, Nat, N, S[S[PP]]]
            implicit def dblSuccCoe : Leibniz[Nothing, Nat, S[S[PP]], N] = force[Nothing, Nat, S[S[PP]], N]

          }
        )
        case _ => None
      }

  }

}

object Nats extends Nats
