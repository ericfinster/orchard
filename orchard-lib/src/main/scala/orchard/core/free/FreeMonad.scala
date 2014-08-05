/**
  * FreeMonad.scala - From scratch Free monad
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.free

import scala.language.higherKinds

import scalaz.{Free => _, _}

sealed abstract class FreeMonad[S[+_], A](implicit ev : Functor[S]) {

  import FreeMonad._

  final def map[B](f : A => B) : FreeMonad[S, B] = 
    this match {
      case Return(a) => Return(f(a))
      case Free(fr) => Free(ev.map(fr)(_.map(f)))
    }

  final def flatMap[B](f : A => FreeMonad[S, B]) : FreeMonad[S, B] =
    this match {
      case Return(a) => f(a)
      case Free(fr) => Free(ev.map(fr)(_.flatMap(f)))
    }


}

object FreeMonad extends FreeMonadInstances {

  case class Return[S[+_] : Functor, A](a : A) extends FreeMonad[S, A]
  case class Free[S[+_] : Functor, A](fr : S[FreeMonad[S, A]]) extends FreeMonad[S, A]

}

trait FreeMonadInstances {

  implicit def freeMonadIsFunctor[S[+_]](implicit ev : Functor[S]) : Monad[({ type L[A] = FreeMonad[S, A]})#L] = 
    new Monad[({ type L[A] = FreeMonad[S, A]})#L] {

      import FreeMonad._

      def point[A](a : => A) : FreeMonad[S, A] = 
        Return[S, A](a)

      def bind[A, B](fa : FreeMonad[S, A])(f : A => FreeMonad[S, B]) : FreeMonad[S, B] = 
        fa.flatMap(f)

    }

}
