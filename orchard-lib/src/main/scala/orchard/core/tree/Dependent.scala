/**
  * Dependent.scala - Playing with dependent types
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import scala.language.higherKinds
import scala.language.implicitConversions

trait SliceAt[-I <: AnyRef] {

  trait LocalType {

    type TypeAt[+_ <: I]

  }

  //============================================================================================
  // PI TYPES
  //

  trait Pi[+S <: LocalType] {

    def eval[J <: I, R >: S#TypeAt[J]] : R

  }

  //============================================================================================
  // SIGMA TYPES
  //

  abstract class Sigma[+S <: LocalType, +J <: I] {

    type Index <: S
    val witness : Index#TypeAt[J]

  }

  object Sigma {

    def apply[S <: LocalType, J <: I](wit : S#TypeAt[J]) : Sigma[S, J] = 
      new Sigma[S, J] {
        type Index = S
        val witness = wit
      }

  }

  //============================================================================================
  // LOCAL (FIBERWISE) HOM
  //

  trait Hom[-S <: LocalType, +T <: LocalType] extends LocalType {

    type TypeAt[+J <: I] <: S#TypeAt[I] => T#TypeAt[J]

    def coerce[J <: I](i : TypeAt[J]) : S#TypeAt[I] => T#TypeAt[J]

  }

  implicit def homCoerce[S <: LocalType, T <: LocalType, J <:I](
    f : S#TypeAt[J] => T#TypeAt[J]
  ) : Hom[S, T]#TypeAt[J]

  trait IFunction[-S <: LocalType, +T <: LocalType] extends Pi[Hom[S, T]] {

    def apply[J <: I](in : S#TypeAt[J]) : T#TypeAt[J] = eval[J, Hom[S, T]#TypeAt[J]](in)
    def apply[J <: I](el : Sigma[S, J]) : Sigma[T, J] = Sigma[T, J](this(el.witness))

  }

  //============================================================================================
  // INDEXED FUNCTORS AND MONADS
  //

  trait IFunctor[F[+_ <: LocalType] <: LocalType] {
    def imap[S <: LocalType, T <: LocalType](f : IFunction[S, T]) : IFunction[F[S], F[T]]
  }

  abstract class IMonad[M[+_ <: LocalType] <: LocalType] extends IFunctor[M] {

    def ireturn[S <: LocalType] : IFunction[S, M[S]]
    def ijoin[S <: LocalType] : IFunction[M[M[S]], M[S]]

  }

  //============================================================================================
  // THE IDENTITY MONAD
  //

  trait Id[+S <: LocalType] extends LocalType {

    type TypeAt[+J <: I] <: S#TypeAt[J]

  }

  implicit def idCoerce[S <: LocalType, J <: I](s : S#TypeAt[J]) : Id[S]#TypeAt[J]

  implicit def idIsMonad : IMonad[Id] = 
    new IMonad[Id] {

      def imap[S <: LocalType, T <: LocalType](f : IFunction[S, T]) : IFunction[Id[S], Id[T]] =
        new IFunction[Id[S], Id[T]] {
          def eval[J <: I, R >: Hom[Id[S], Id[T]]#TypeAt[J]] : R = {
            implicitly[Hom[Id[S], Id[T]]#TypeAt[J] <:< (Id[S]#TypeAt[I] => Id[T]#TypeAt[J])]
            implicitly[Hom[S, T]#TypeAt[J] <:< (S#TypeAt[I] => T#TypeAt[J])]
            implicitly[Id[S]#TypeAt[J] <:< Id[S]#TypeAt[I]]
            implicitly[Id[S]#TypeAt[J] <:< S#TypeAt[J]]
            implicitly[Id[T]#TypeAt[J] <:< T#TypeAt[J]]

            ((x : Id[S]#TypeAt[J]) => (f.eval[J, Hom[S, T]#TypeAt[J]])(x)) : Hom[Id[S], Id[T]]#TypeAt[J]

          }
        }


      def ireturn[S <: LocalType] : IFunction[S, Id[S]] =
        new IFunction[S, Id[S]] {
          def eval[J <: I, R >: Hom[S, Id[S]]#TypeAt[J]] : R = ??? //identity
        }


      def ijoin[S <: LocalType] : IFunction[Id[Id[S]], Id[S]] = 
        new IFunction[Id[Id[S]], Id[S]] {
          def eval[J <: I, R >: Hom[Id[Id[S]], Id[S]]#TypeAt[J]] : R = ??? //identity
        }

    }

}
