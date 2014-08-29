/**
  * SliceFix.scala - The fixed point of the slice construction
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.cell

import scala.language.higherKinds
import scala.language.implicitConversions

import scalaz._
import scalaz.syntax.monad._

abstract class SliceFix[F[+_], +A]
case class Obj[F[+_], +A](a : A) extends SliceFix[F, A]
case class Cup[F[+_], +A](a : A, fa : F[A]) extends SliceFix[F, A]
case class Fix[F[+_], +A](fa : F[A], sf : SliceFix[({ type L[+A] = Slice[F, A] })#L, A]) extends SliceFix[F, A]

trait SliceFixInstances {

  // implicit def sliceFixIsFunctor[F[+_], A](implicit F : Functor[F]) : Functor[({ type L[A] = SliceFix[F, A] })#L] =
  //   new Functor[({ type L[A] = SliceFix[F, A] })#L] {

  //     type SliceType[+A] = Slice[F, A]
  //     type SliceFixType[+A] = SliceFix[SliceType, A]

  //     val SF = implicitly[Functor[SliceFixType]]

  //     def map[A, B](fa : SliceFix[F, A])(f : A => B) : SliceFix[F, B] =
  //       fa match {
  //         case Obj(a) => Obj(f(a))
  //         case Cup(a, fa) => Cup(f(a), fa map f)
  //         case Fix(fa, sf : SliceFixType[A]) => Fix(fa map f, SF.map(sf)(f)) 
  //       }

  //   }


}
