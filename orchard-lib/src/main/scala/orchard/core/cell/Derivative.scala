/**
  * Derivative.scala - Type Derivatives for the Slice Construction
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.cell

import scala.language.higherKinds
import scala.language.implicitConversions

import scalaz.Id._

trait Differentiable[T[_]] {

  type Derivative[_] 

  def fill[A](d : Derivative[A], a : A) : T[A]

  type Zipper[A] = List[(A, Derivative[A])]
  type Context[A] = (T[A], Zipper[A])

}

object Differentiable {

  type Empty[A] = Nothing

  implicit def emptyDifferentiable : Differentiable[Empty] =
    new Differentiable[Empty] {

      type Derivative[X] = Empty[X]

      def fill[A](e : Empty[A], a : A) : Empty[A] = ???

    }

  implicit def identityDifferentiable : Differentiable[Id] = 
    new Differentiable[Id] {

      type Derivative[X] = Unit

      def fill[A](u : Unit, a : A) : Id[A] = a

    }

  implicit def sliceDifferentiable[F[_]](implicit diffF : Differentiable[F]) : Differentiable[({ type L[A] = Slice[F, A] })#L] =
    new Differentiable[({ type L[A] = Slice[F, A] })#L] {

      type FZipper[A] = diffF.Zipper[A]
      type Derivative[A] = (F[Slice[F, A]], FZipper[A])

      def fill[A](d : Derivative[A], a : A) = ???

    }

}
