/**
  * TreeIndex.scala - Indexing of Tree-related types
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import scala.language.higherKinds
import scala.language.implicitConversions

import scalaz.Id._
import scalaz.Leibniz._

sealed trait TreeIndex {

  type Self <: TreeIndex
  type Pred <: TreeIndex

  type Tree[+_]
  type Context[+_]
  type Derivative[+_]
  type Zipper[+_]

  type Direction
  type Address = List[Direction]

  def toInt : Int

}

case object ZT extends TreeIndex {

  type Self = this.type
  type Pred = Nothing

  type Tree[+A] = Id[A]
  type Context[+A] = Unit
  type Derivative[+A] = Unit
  type Zipper[+A] = (Unit, Unit)

  type Direction = Nothing

  def toInt : Int = 0
  override def toString = toInt.toString

}

case class ST[P <: TreeIndex](pred : P) extends TreeIndex {

  type Self = ST[Pred]
  type Pred = P

  type Tree[+A] = Slice[Pred#Tree, A]
  type Context[+A] = List[(A, Pred#Derivative[Tree[A]])]
  type Derivative[+A] = (Pred#Tree[Tree[A]], Context[A])
  type Zipper[+A] = (Tree[A], Context[A])

  type Direction = List[Pred#Direction]

  def toInt : Int = pred.toInt + 1
  override def toString = toInt.toString

}


object TreeIndex {

  type _0 = ZT.type
  type _1 = ST[_0]
  type _2 = ST[_1]
  type _3 = ST[_2]
  type _4 = ST[_3]
  type _5 = ST[_4]

  type Tree[N <: TreeIndex, +A] = N#Tree[A]
  type Context[N <: TreeIndex, +A] = N#Context[A]
  type Derivative[N <: TreeIndex, +A] = N#Derivative[A]
  type Zipper[N <: TreeIndex, +A] = N#Zipper[A]

  type Direction[N <: TreeIndex] = N#Direction

  object IsZeroIndex {
    def unapply[N <: TreeIndex](n : N) : Option[ZeroMatch[N]] =
      n match {
        case ZT => ??? //Some(new ZeroMatch[N] { })
        case _ => None
      }
  }

  object IsOneIndex {
    def unapply[N <: TreeIndex](n : N) : Option[OneMatch[N]] = 
      n match {
        case ST(ZT) => ??? //Some(new OneMatch[N] { })
        case _ => None
      }
  }

  object IsSuccIndex {
    def unapply[N <: TreeIndex](n : N) : Option[SuccMatch[N]] = 
      n match {
        case ST(pred) => ???
          // Some(new SuccMatch[N] { })
        case _ => None
      }
  }

  object IsDblSuccIndex {
    def unapply[N <: TreeIndex](n : N) : Option[DblSuccMatch[N]] = 
      n match {
        case ST(ST(ppred)) => ???
          // Some(new DblSuccMatch[N] { })
        case _ => None
      }
  }

  trait ZeroMatch[N <: TreeIndex]  {

    implicit def coh : N === ZT.type
    implicit def coe : ZT.type === N

  }

  trait OneMatch[N <: TreeIndex] {

    implicit def coh : N === ST[ZT.type]
    implicit def coe : ST[ZT.type] === N

  }

  trait SuccMatch[N <: TreeIndex] {

    type P <: TreeIndex
    val p : P

    implicit def coh : N === ST[P]
    implicit def coe : ST[P] === N

  }

  trait DblSuccMatch[N <: TreeIndex] {

    type PP <: TreeIndex
    val pp : PP

    type P = ST[PP]
    def p : P = ST(pp)

    implicit def coh : N === ST[ST[PP]]
    implicit def coe : ST[ST[PP]] === N

  }

  def rewrite[F[_], A, B](fa : F[A])(implicit eq : A === B) : F[B] = eq.subst[F](fa)

  // Implicit Equalities
  implicit def liftTree[M <: TreeIndex, N <: TreeIndex, A](implicit eq : M === N) : Tree[M, A] === Tree[N, A] =
    force[Nothing, Any, Tree[M, A], Tree[N, A]]

  implicit def liftDerv[M <: TreeIndex, N <: TreeIndex, A](implicit eq : M === N) : Derivative[M, A] === Derivative[N, A] = 
    force[Nothing, Any, Derivative[M, A], Derivative[N, A]]

  implicit def liftCntxt[M <: TreeIndex, N <: TreeIndex, A](implicit eq : M === N) : Context[M, A] === Context[N, A] = 
    force[Nothing, Any, Context[M, A], Context[N, A]]

  implicit def liftZipper[M <: TreeIndex, N <: TreeIndex, A](implicit eq : M === N) : Zipper[M, A] === Zipper[N, A] = 
    force[Nothing, Any, Zipper[M, A], Zipper[N, A]]

  implicit def liftDir[M <: TreeIndex, N <: TreeIndex](implicit eq : M === N) : Direction[M] === Direction[N] =
    force[Nothing, Any, Direction[M], Direction[N]]

  // Implicit Conversions
  implicit def treeCoerce[M <: TreeIndex, N <: TreeIndex, A](t : Tree[M, A])(implicit eq : M === N) : Tree[N, A] =
    subst(t)(implicitly[Tree[M, A] === Tree[N, A]])

  implicit def derivCoerce[M <: TreeIndex, N <: TreeIndex, A](d : Derivative[M, A])(implicit eq :M === N) : Derivative[N, A] = 
    subst(d)(implicitly[Derivative[M, A] === Derivative[N, A]])

  implicit def contextCoerce[M <: TreeIndex, N <: TreeIndex, A](c : Context[M, A])(implicit eq : M === N) : Context[N, A] = 
    subst(c)(implicitly[Context[M, A] === Context[N, A]])

  implicit def zipperCoerce[M <: TreeIndex, N <: TreeIndex, A](z : Zipper[M, A])(implicit eq : M === N) : Zipper[N, A] = 
    subst(z)(implicitly[Zipper[M, A] === Zipper[N, A]])

  implicit def dirCoerce[M <: TreeIndex, N <: TreeIndex](d : Direction[M])(implicit eq : M === N) : Direction[N] =
    subst(d)(implicitly[Direction[M] === Direction[N]])

}
