/**
  * Slice.scala - The Slice Construction
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.cell

import scala.language.higherKinds
import scala.language.implicitConversions

import scalaz._
import Leibniz._
import scalaz.syntax.monad._

import orchard.core.util._
import ErrorM._
import Nats._

sealed trait Slice[F[_], +A]

case class Cap[F[+_], +A]() extends Slice[F, A] 
case class Joint[F[+_], +A](a : A, fsa : F[Slice[F, A]]) extends Slice[F, A] 

trait SliceInstances {

  implicit def sliceIsTraverse[F[+_], A](implicit F : Traverse[F]) : Traverse[({ type L[A] = Slice[F, A] })#L] =
    new Traverse[({ type L[A] = Slice[F, A] })#L] {

      type SliceType[+A] = Slice[F, A]

      val SF = implicitly[Functor[SliceType]]

      override def map[A, B](fa : Slice[F, A])(f : A => B) : Slice[F, B] =
        fa match {
          case Cap() => Cap()
          case Joint(a, fsa) => Joint[F, B](f(a), fsa map (SF.map(_)(f)))
        }

      def traverseImpl[G[_], A, B](fa : SliceType[A])(f : A => G[B])(implicit apG : Applicative[G]) : G[SliceType[B]] = {
        import apG.{traverse => _, _}

        fa match {
          case Cap() => pure(Cap())
          case Joint(a, fsa) => {

            val jointCons : G[(B, F[Slice[F, B]]) => Slice[F, B]] = 
              point((b : B, fsb : F[Slice[F, B]]) => Joint(b, fsb))

            ap2(f(a), F.sequence(fsa map (t => traverse(t)(f))))(jointCons)

          }
        }
      }

    }

}

object Slice extends SliceInstances {

  sealed trait SliceOf[F[+_], G[+_]]

  trait ZeroSliceOf[F[+_], G[+_]] extends SliceOf[F, G] {

    def coh[A] : F[A] === G[A]
    def coe[A] : G[A] === F[A] = 
      symm[Nothing, Any, F[A], G[A]](coh[A])

  }

  trait SuccSliceOf[F[+_], G[+_]] extends SliceOf[F, G] {

    type P[+_]

    implicit val prevSlice : SliceOf[F, P]

    def coh[A] : Slice[P, A] === G[A]
    def coe[A] : G[A] === Slice[P, A] = 
      symm[Nothing, Any, Slice[P, A], G[A]](coh[A])

  }


  implicit def sliceZero[F[+_]] : SliceOf[F, F] = 
    new ZeroSliceOf[F, F] { 

      def coh[A] : F[A] === F[A] = refl[F[A]]

    }

  implicit def sliceSucc[F[+_], G[+_]](implicit sl : SliceOf[F, G]) : SuccSliceOf[F, ({ type L[+X] = Slice[G, X] })#L] =
    new SuccSliceOf[F, ({ type L[+X] = Slice[G, X] })#L] {

      type P[+A] = G[A]

      implicit val prevSlice = sl

      def coh[A] : Slice[P, A] === Slice[P, A] = refl[Slice[P, A]]

    }

  implicit class SliceOps[F[+_], +A](s : Slice[F, A]) {

    def isCap : Boolean = 
      s match {
        case Cap() => true
        case _ => false
      }

  }
}
