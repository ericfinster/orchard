/**
  * MonadUtils.scala - Monad utility stuff
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.typechecker

import scala.language.higherKinds

import scalaz._

import orchard.core.util._
import ErrorM._

object MonadUtils {

  implicit val errorIsMonad : Monad[Error] =
    new Monad[Error] {
      def point[A](a : => A) : Error[A] = succeed(a)
      def bind[A, B](fa : Error[A])(f : A => Error[B]) : Error[B] = 
        fa match {
          case Right(a) => f(a)
          case Left(msg) => fail(msg)
        }
    }

  abstract class ErrorLifts[F[_[_], _]](implicit ev : MonadTrans[F]) {

    def attempt[A](e : Error[A]) : F[Error, A] = ev.liftM(e)
    def succeed[A](a : A) : F[Error, A] = ev(errorIsMonad).point(a)
    def fail[A](msg : String) : F[Error, A] = attempt(Left(msg))

  }

  implicit def roseTreeTraverse[N] : Traverse[({ type L[M] = RoseTree[M, N] })#L] =
    new Traverse[({ type L[M] = RoseTree[M, N] })#L] {

      type FixedRose[M] = RoseTree[M, N]

      def traverseImpl[G[_], A, B](fa : FixedRose[A])(f : A => G[B])(
        implicit ev : Applicative[G]
      ) : G[FixedRose[B]] = {

        import ev.applicativeSyntax._
        import scalaz.std.vector._
        import scalaz.syntax.traverse._

        fa match {
          case Rose(n) => point(Rose(n))
          case Branch(a, brs) => {
            ev.ap2(f(a), (brs map (traverseImpl(_)(f))).sequence)(
              pure((v : B, bs : Vector[FixedRose[B]]) => Branch(v, bs) : FixedRose[B])
            )
          }
        }
      }

    }

}
