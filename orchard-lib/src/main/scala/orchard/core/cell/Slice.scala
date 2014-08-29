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
import scalaz.syntax.monad._

import orchard.core.util._
import ErrorM._
import Nats._

sealed trait Slice[F[_], +A]

case class Cap[F[+_], +A]() extends Slice[F, A]
case class Joint[F[+_], +A](a : A, fsa : F[Slice[F, A]]) extends Slice[F, A]

object Slice {


}

// trait SliceFunctions {

// }

// trait SliceInstances {

//   implicit def sliceIsFunctor[F[+_], A](implicit F : Functor[F]) : Functor[({ type L[A] = Slice[F, A] })#L] =
//     new Functor[({ type L[A] = Slice[F, A] })#L] {

//       type SliceType[+A] = Slice[F, A]

//       val SF = implicitly[Functor[SliceType]]

//       def map[A, B](fa : Slice[F, A])(f : A => B) : Slice[F, B] =
//         fa match {
//           case Cap() => Cap()
//           case Joint(a, fsa) => Joint[F, B](f(a), fsa map (SF.map(_)(f)))
//         }

//     }


// }


// object SliceExamples {



// }

