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

  object IsZeroIndex {
    def unapply[N <: TreeIndex](n : N) : Option[ZeroImplicits[N]] =
      n match {
        case ZT => Some(new ZeroImplicits[N] { })
        case _ => None
      }
  }

  object IsOneIndex {
    def unapply[N <: TreeIndex](n : N) : Option[OneImplicits[N]] = 
      n match {
        case ST(ZT) => Some(new OneImplicits[N] { })
        case _ => None
      }
  }

  object IsSuccIndex {
    def unapply[N <: TreeIndex](n : N) : Option[SuccImplicits[N]] = 
      n match {
        case ST(pred) => 
          Some(new SuccImplicits[N] { val p = pred.asInstanceOf[P] })
        case _ => None
      }
  }

  object IsDblSuccIndex {
    def unapply[N <: TreeIndex](n : N) : Option[GteTwoImplicits[N]] = 
      n match {
        case ST(ST(ppred)) =>
          Some(new GteTwoImplicits[N] { val pp = ppred.asInstanceOf[PP] })
        case _ => None
      }
  }

  trait TreeConversions[N <: TreeIndex, M <: TreeIndex] {

    implicit def rewrite[F[_], A, B](fa : F[A])(implicit eq : A === B) : F[B] = eq.subst[F](fa)

    implicit def treeCohEq[A] : N#Tree[A] === M#Tree[A] = force[Nothing, Any, N#Tree[A], M#Tree[A]]
    implicit def contextCohEq[A] : N#Context[A] === M#Context[A] = force[Nothing, Any, N#Context[A], M#Context[A]]
    implicit def derivativeCohEq[A] : N#Derivative[A] === M#Derivative[A] = force[Nothing, Any, N#Derivative[A], M#Derivative[A]]
    implicit def zipperCohEq[A] : N#Zipper[A] === M#Zipper[A] = force[Nothing, Any, N#Zipper[A], M#Zipper[A]]
    implicit def directionCohEq[A] : N#Direction === M#Direction = force[Nothing, Any, N#Direction, M#Direction]
    implicit def addressCohEq[A] : N#Address === M#Address = force[Nothing, Any, N#Address, M#Address]

    implicit def treeCoeEq[A] : M#Tree[A] === N#Tree[A] = force[Nothing, Any, M#Tree[A], N#Tree[A]]
    implicit def contextCoeEq[A] : M#Context[A] === N#Context[A] = force[Nothing, Any, M#Context[A], N#Context[A]]
    implicit def derivativeCoeEq[A] : M#Derivative[A] === N#Derivative[A] = force[Nothing, Any, M#Derivative[A], N#Derivative[A]]
    implicit def zipperCoeEq[A] : M#Zipper[A] === N#Zipper[A] = force[Nothing, Any, M#Zipper[A], N#Zipper[A]]
    implicit def directionCoeEq[A] : M#Direction === N#Direction = force[Nothing, Any, M#Direction, N#Direction]
    implicit def addressCoeEq[A] : M#Address === N#Address = force[Nothing, Any, M#Address, N#Address]

    implicit def treeCoh[A](nt : N#Tree[A]) : M#Tree[A] = rewrite[Id, N#Tree[A], M#Tree[A]](nt)
    implicit def contextCoh[A](nt : N#Context[A]) : M#Context[A] = rewrite[Id, N#Context[A], M#Context[A]](nt)
    implicit def derivativeCoh[A](nt : N#Derivative[A]) : M#Derivative[A] = rewrite[Id, N#Derivative[A], M#Derivative[A]](nt)
    implicit def zipperCoh[A](nt : N#Zipper[A]) : M#Zipper[A] = rewrite[Id, N#Zipper[A], M#Zipper[A]](nt)
    implicit def directionCoh(nd : N#Direction) : M#Direction = rewrite[Id, N#Direction, M#Direction](nd)
    implicit def addressCoh(nd : N#Address) : M#Address = rewrite[Id, N#Address, M#Address](nd)

    implicit def treeCoe[A](mt : M#Tree[A]) : N#Tree[A] = rewrite[Id, M#Tree[A], N#Tree[A]](mt)
    implicit def contextCoe[A](mt : M#Context[A]) : N#Context[A] = rewrite[Id, M#Context[A], N#Context[A]](mt)
    implicit def derivativeCoe[A](mt : M#Derivative[A]) : N#Derivative[A] = rewrite[Id, M#Derivative[A], N#Derivative[A]](mt)
    implicit def zipperCoe[A](mt : M#Zipper[A]) : N#Zipper[A] = rewrite[Id, M#Zipper[A], N#Zipper[A]](mt)
    implicit def directionCoe(md : M#Direction) : N#Direction = rewrite[Id, M#Direction, N#Direction](md)
    implicit def addressCoe(md : M#Address) : N#Address = rewrite[Id, M#Address, N#Address](md)

    // Can't do this because the names then block out the previous conversions ... how to fix it?
    def succConversions : TreeConversions[ST[N], ST[M]] = new TreeConversions[ST[N], ST[M]] { }

    // The point is that you should have the leibniz apply to the index, then implicits to generate
    // leibniz's for the derived types.  Then the successor conversions just come from lifting a given
    // index equality into the succ type constructor, and all the old conversions apply.

  }

  trait ZeroImplicits[N <: TreeIndex] extends TreeConversions[N, _0]
  trait OneImplicits[N <: TreeIndex] extends TreeConversions[N, _1]

  trait SuccImplicits[N <: TreeIndex] extends TreeConversions[N, ST[N#Pred]] {

    type P = N#Pred

    val p : P

    type PTree[+A] = P#Tree[A]
    type STree[+A] = ST[P]#Tree[A]

  }

  trait GteTwoImplicits[N <: TreeIndex] extends TreeConversions[N, ST[ST[N#Pred#Pred]]] {

    type PP = N#Pred#Pred
    type P = ST[PP]

    type FS[+A] = Slice[PP#Tree, A]

    val pp : PP

    def p : ST[PP] = ST(pp)

  }

}
