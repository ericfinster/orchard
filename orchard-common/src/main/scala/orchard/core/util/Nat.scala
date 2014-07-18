/**
  * Nat.scala - Type-level natural numbers
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.util

sealed trait Nat { type Pred <: Nat; type Self <: Nat; def toInt : Int }
case object Z extends Nat { type Pred = Nothing ; type Self = Z.type ; def toInt = 0 }
case class S[N <: Nat](pred : N) extends Nat { type Pred = N ; type Self = S[N] ; def toInt = pred.toInt + 1 }

trait IsZero[N <: Nat]

object IsZero {
  implicit def zeroIsZero : IsZero[Z.type] = new IsZero[Z.type] { }

  def unapply[N <: Nat](n : N) : Option[IsZero[N]] = 
    n match {
      case Z => Some(new IsZero[N] { })
      case _ => None
    }
}

trait HasPred[A <: Nat] {
  type Pred <: Nat
}

trait IsPred[P <: Nat, N <: Nat]

object HasPred {
  implicit def pred[P <: Nat, N <: Nat](implicit pred : IsPred[P, N]) = new HasPred[N] {
    type Out = P
  }

  def unapply[N <: Nat](n : N) : Option[HasPred[N]] =
    n match {
      case Z => None
      case S(p) => Some(new HasPred[N] { type Out = N#Pred })
    }
}

object IsPred {
  implicit def pred[N <: Nat] = new IsPred[N, S[N]] {}
}

object Zero {
  def unapply[N <: Nat](n : N) : Option[IsZero[N]] =
    n match {
      case Z => Some(new IsZero[N] { })
      case S(_) => None
    }
}

object Succ {
  def unapply[N <: Nat](n : N) : Option[(Nat, HasPred[N])] = 
    n match {
      case Z => None
      case S(p) => Some((p, new HasPred[N] { type Out = p.Self }))
    }
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

  val _0 = Z
  val _1 = S(_0)
  val _2 = S(_1)
  val _3 = S(_2)
  val _4 = S(_3)
  val _5 = S(_4)
  val _6 = S(_5)
  val _7 = S(_6)
  val _8 = S(_7)
  val _9 = S(_8)

  def fromInt(n : Int) : Nat = 
    if (n <= 0) {
      _0
    } else {
      S(fromInt(n - 1))
    }

  def getPred[N <: Nat : HasPred](n : N) : N#Pred = {
    n match {
      case S(p) => p.asInstanceOf[N#Pred]
    }
  }
}

object Nats extends Nats

sealed trait HDN[D <: Nat] { def isZero : Boolean }
case object HZero extends HDN[Nats._0] { override def toString = "0" ; def isZero = true }
case class HUnit[D <: Nat](l : List[HDN[D]]) extends HDN[S[D]] {
  def isZero = l.length == 0

  override def toString = {
    val temp = 
      if (l.length > 1) {
        l.head.toString :: (l.tail map ("," ++ _.toString))
      } else {
        l map (_.toString)
      }

    (("[" /: temp) (_ ++ _)) ++ "]"
  }
}

object HDN {
  def apply[D <: Nat](d : D) : HDN[D] = 
    d match {
      case Z => HZero.asInstanceOf[HDN[D]]
      case S(p) => HUnit[D#Pred](List()).asInstanceOf[HDN[D]]
    }

  implicit class HDNOps[D <: Nat](hdn : HDN[S[D]]) {
    def +:(h : HDN[D]) : HDN[S[D]] =
      hdn match {
        case HUnit(l) => HUnit(h :: l)
      }
  }
}
