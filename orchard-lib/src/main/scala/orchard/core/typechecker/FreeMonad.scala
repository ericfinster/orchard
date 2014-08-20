/**
  * FreeMonad.scala - From scratch Free monad
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.typechecker

import scala.language.higherKinds

import scalaz.{Free => _, _}

sealed abstract class Free[S[+_], A](implicit ev : Functor[S]) {

  import Free._

  final def map[B](f : A => B) : Free[S, B] = 
    this match {
      case Return(a) => Return(f(a))
      case Join(fr) => Join(ev.map(fr)(_.map(f)))
    }

  final def flatMap[B](f : A => Free[S, B]) : Free[S, B] =
    this match {
      case Return(a) => f(a)
      case Join(fr) => Join(ev.map(fr)(_.flatMap(f)))
    }


}

object Free extends FreeInstances {

  case class Return[S[+_] : Functor, A](a : A) extends Free[S, A]
  case class Join[S[+_] : Functor, A](fr : S[Free[S, A]]) extends Free[S, A]

}

trait FreeInstances {

  implicit def freeIsMonad[S[+_]](implicit ev : Functor[S]) : Monad[({ type L[A] = Free[S, A]})#L] = 
    new Monad[({ type L[A] = Free[S, A]})#L] {

      import Free._

      def point[A](a : => A) : Free[S, A] = 
        Return[S, A](a)

      def bind[A, B](fa : Free[S, A])(f : A => Free[S, B]) : Free[S, B] = 
        fa.flatMap(f)

    }

}
