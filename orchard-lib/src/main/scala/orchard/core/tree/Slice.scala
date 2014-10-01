/**
  * Slice.scala - The Slice Construction
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import scala.language.higherKinds
import scala.language.implicitConversions

import scalaz._
import scalaz.syntax.applicative._

sealed abstract class Slice[F[+_], +L, +N] {

  def map[M](f : N => M)(implicit isF : Functor[F]) : Slice[F, L, M] =
    this match {
      case Leaf(l) => Leaf(l) 
      case Node(n, fsa) => Node(f(n), fsa map (_.map(f)))
    }

  def traverse[G[_], K >: L, M](f : N => G[M])(implicit isA : Applicative[G], isT : Traverse[F]) : G[Slice[F, K, M]] = {
    import isA.{traverse => _, _}

    this match {
      case Leaf(l) => point(Leaf(l))
      case Node(n, fsn) => {

        val nodeCons : G[(M , F[Slice[F, L, M]]) => Slice[F, K, M]] = 
          point((m : M, fsm : F[Slice[F, L, M]]) => Node(m, fsm))

        ap2(f(n), isT.traverse(fsn)(_.traverse(f)))(nodeCons)

      }
    }
  }

}

case class Leaf[F[+_], +L, +N](l : L) extends Slice[F, L, Nothing] 
case class Node[F[+_], +L, +N](n : N, fsa : F[Slice[F, L, N]]) extends Slice[F, L, N] 

trait SliceInstances {

  implicit def sliceIsTraverse[F[+_], L](implicit F : Traverse[F]) : Traverse[({ type U[N] = Slice[F, L, N] })#U] =
    new Traverse[({ type U[N] = Slice[F, L, N] })#U] {

      type SliceType[+N] = Slice[F, L, N]

      override def map[N, M](fn : Slice[F, L, N])(f : N => M) : Slice[F, L, M] = 
        fn.map(f)

      def traverseImpl[G[_], N, M](fn : SliceType[N])(f : N => G[M])(implicit apG : Applicative[G]) : G[SliceType[M]] = 
        fn.traverse(f)

    }

}

object Slice extends SliceInstances {

//   sealed trait SliceOf[F[+_], G[+_]]

//   trait ZeroSliceOf[F[+_], G[+_]] extends SliceOf[F, G] {

//     def coh[A] : F[A] === G[A]
//     def coe[A] : G[A] === F[A] = 
//       symm[Nothing, Any, F[A], G[A]](coh[A])

//   }

//   trait SuccSliceOf[F[+_], G[+_]] extends SliceOf[F, G] {

//     type P[+_]

//     implicit val prevSlice : SliceOf[F, P]

//     def coh[A] : Slice[P, A] === G[A]
//     def coe[A] : G[A] === Slice[P, A] = 
//       symm[Nothing, Any, Slice[P, A], G[A]](coh[A])

//   }


//   implicit def sliceZero[F[+_]] : SliceOf[F, F] = 
//     new ZeroSliceOf[F, F] { 

//       def coh[A] : F[A] === F[A] = refl[F[A]]

//     }

//   implicit def sliceSucc[F[+_], G[+_]](implicit sl : SliceOf[F, G]) : SuccSliceOf[F, ({ type L[+X] = Slice[G, X] })#L] =
//     new SuccSliceOf[F, ({ type L[+X] = Slice[G, X] })#L] {

//       type P[+A] = G[A]

//       implicit val prevSlice = sl

//       def coh[A] : Slice[P, A] === Slice[P, A] = refl[Slice[P, A]]

//     }

//   implicit class SliceOps[F[+_], +A](s : Slice[F, A]) {

//     def isCap : Boolean = 
//       s match {
//         case Cap() => true
//         case _ => false
//       }

//     def sliceHello : Unit = ()

//   }
}
