/**
  * Index.scala - Again playing with indexing and the free monad
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import scala.language.higherKinds
import scala.language.implicitConversions

//============================================================================================
// DEFINITONS GENERIC OVER THE INDEX
//

trait SliceOver[I <: AnyRef] {

  trait IndexedType {

    type TypeAt[J <: I] 

  }

  trait Pi[E <: IndexedType] {

    def eval[J <: I] : E#TypeAt[J]

  }

  trait Sigma[E <: IndexedType, J <: I] {

    type IndexType = J

    val witness : E#TypeAt[J]

  }

  object Sigma {

    def apply[E <: IndexedType, J <: I](wit : E#TypeAt[J]) : Sigma[E, J] =
      new Sigma[E, J] {
        val witness = wit
      }

  }

  //============================================================================================
  // THE TERMINAL OBJECT
  //

  final class Term extends IndexedType {

    type TypeAt[J <: I] = Unit

  }

  //============================================================================================
  // LOCAL (FIBERWISE) HOM
  //

  trait Hom[S <: IndexedType, T <: IndexedType] extends IndexedType {

    final type TypeAt[J <: I] = S#TypeAt[J] => T#TypeAt[J]

  }

  trait IFunction[E <: IndexedType, F <: IndexedType] extends Pi[Hom[E, F]] {

    def apply[J <: I](in : E#TypeAt[J]) : F#TypeAt[J] = eval[J](in)
    def apply[J <: I](el : Sigma[E, J]) : Sigma[F, J] = Sigma[F, J](this(el.witness))

  }

  //============================================================================================
  // FUNCTORS AND MONADS
  //

  trait IFunctor[F[_ <: IndexedType] <: IndexedType] {
    def iMap[S <: IndexedType, T <: IndexedType](f : IFunction[S, T]) : IFunction[F[S], F[T]]
  }

  abstract class IMonad[M[_ <: IndexedType] <: IndexedType] extends IFunctor[M] {

    def iReturn[E <: IndexedType] : IFunction[E, M[E]]
    def iJoin[E <: IndexedType] : IFunction[M[M[E]], M[E]]

  }

  //============================================================================================
  // THE IDENTITY MONAD
  //

  class Id[E <: IndexedType] extends IndexedType {

    type TypeAt[J <: I] = E#TypeAt[J]

  }

  implicit def idIsMonad : IMonad[Id] = 
    new IMonad[Id] {

      def iMap[S <: IndexedType, T <: IndexedType](f : IFunction[S, T]) : IFunction[Id[S], Id[T]] =
        new IFunction[Id[S], Id[T]] {
          def eval[J <: I] : Hom[Id[S], Id[T]]#TypeAt[J] = f.eval[J]
        }


      def iReturn[E <: IndexedType] : IFunction[E, Id[E]] = 
        new IFunction[E, Id[E]] {
          def eval[J <: I] : Hom[E, Id[E]]#TypeAt[J] = identity
        }


      def iJoin[E <: IndexedType] : IFunction[Id[Id[E]], Id[E]] =
        new IFunction[Id[Id[E]], Id[E]] {
          def eval[J <: I] : Hom[Id[Id[E]], Id[E]]#TypeAt[J] = identity
        }

    }

  //============================================================================================
  // THE FREE MONAD
  //

  sealed abstract class Free[F[_ <: IndexedType] <: IndexedType, E <: IndexedType, J <: I](implicit ev : IFunctor[F]) extends IndexedType {

    final type TypeAt[J <: I] = Free[F, E, J]

  }

  case class Return[F[_ <: IndexedType] <: IndexedType, E <: IndexedType, J <: I](el : Sigma[E, J])(implicit ev : IFunctor[F]) extends Free[F, E, J]
  case class Fix[F[_ <: IndexedType] <: IndexedType, E <: IndexedType, J <: I](el : Sigma[F[Free[F, E, I]], J])(implicit ev : IFunctor[F]) extends Free[F, E, J]

  implicit def freeIsMonad[F[_ <: IndexedType] <: IndexedType](implicit ev : IFunctor[F]) 
      : IMonad[({ type L[E <: IndexedType] = Free[F, E, I] })#L] =
    new IMonad[({ type L[E <: IndexedType] = Free[F, E, I] })#L] {

      type FreeM[E <: IndexedType] = Free[F, E, I]

      def iMap[S <: IndexedType, T <: IndexedType](f : IFunction[S, T]) : IFunction[FreeM[S], FreeM[T]] = 
        new IFunction[FreeM[S], FreeM[T]] {
          def eval[J <: I] : Hom[FreeM[S], FreeM[T]]#TypeAt[J] = ((fr : Free[F, S, J]) => {
            fr match {
              case Return(el) => Return(f(el))
              case Fix(el) => Fix(ev.iMap(iMap(f))(el))
            }
          })
        }

      def iReturn[E <: IndexedType] : IFunction[E, FreeM[E]] = 
        new IFunction[E, FreeM[E]] {

          def eval[J <: I] : Hom[E, FreeM[E]]#TypeAt[J] = ((el : E#TypeAt[J]) => {
            Return(Sigma[E, J](el))
          })

        }

      def iJoin[E <: IndexedType] : IFunction[FreeM[FreeM[E]], FreeM[E]] =
        new IFunction[FreeM[FreeM[E]], FreeM[E]] {
          def eval[J <: I] : Hom[FreeM[FreeM[E]], FreeM[E]]#TypeAt[J] = ((ff : Free[F, FreeM[E], J]) => {
            ff match {
              case Return(el) => el.witness
              case Fix(el) => Fix(ev.iMap(iJoin[E])(el))
            }
          })
        }

    }

  //============================================================================================
  // INTERPRETATION
  //

  implicit def interpretation[M[_ <: IndexedType] <: IndexedType, E <: IndexedType](implicit ev : IMonad[M]) 
      : IFunction[Free[M, E, I], M[E]] =
    new IFunction[Free[M, E, I], M[E]] {

      def eval[J <: I] : Hom[Free[M, E, I], M[E]]#TypeAt[J] = ((fm : Free[M, E, J]) => {
        fm match {
          case Return(el) => ev.iReturn(el.witness)
          case Fix(el) => ev.iJoin(ev.iMap(interpretation[M, E])(el.witness))
        }
      })

    }

  // Right, so I think the idea is something like this: you have a custom version of the free monad
  // class and possibly constructors for the case when the given functor guy is a monad.

  // In this case, you compute the interpretation immediately and somehow store the associated
  // type or value or whatever is appropriate at the same time.

  // type L <: I
  // type G <: IndexedType
  // type R[_ <: IndexedType] <: IndexedType

  // implicit val rIsMonad : IMonad[R]

  // implicitly[Free[R, G, L] <:< Free[R, G, I]]

  //============================================================================================
  // EXAMPLE FREE MONAD USAGE
  //

  // type IdSlice[E <: IndexedType, J <: I] = Free[Id, E, J]

  // def unit[E <: IndexedType, J <: I](el : Sigma[E, J]) : IdSlice[E, J] =
  //   Return[Id, E, J](el)

  // def fix[E <: IndexedType, J <: I, K <: I](el : Sigma[Id[IdSlice[E, K]], J]) : IdSlice[E, J] = 
  //   Fix[Id, E, J, K](el)

}

trait SliceTest {

  // type I <: AnyRef
  // type J <: I

  // object OverI extends SliceOver[I] 
  // import OverI._

  // type E <: IndexedType


//   type M[_ <: IndexedType] <: IndexedType
//   type K = Sigma[M[Term], I]

//   implicit val mIsMonad : IMonad[M]

//   object OverK extends SliceOver[K] {

//     // Now the idea is to give some kind of new definition of the
//     // free monad with a different indexing.  Not quite sure how this
//     // works ....

//     // I think the first step is to show that an indexed type here
//     // gives rise to an indexed type in the previous sense.

//     trait Reindexing[E <: IndexedType] extends OverI.IndexedType {

//       type Test[L <: K] = E#TypeAt[L]
//       type Test2[L <: K] = L#IndexType

//       final type TypeAt[J <: I] = OverI.Sigma[M[OverI.Term], J]

//     }

//     // With this in hand, we should be able to take a local indexed type and apply
//     // the previous free monad to it

//     type SliceM[E <: IndexedType] = OverI.Free[M, Reindexing[E], I]

//     // Well, it's pretty neat that this works, but I think what you probably rather want
//     // to do is to redefine the free monad here such that it performs the reindexing
//     // as you go.  That is, such that the constructors take the unravelled version.

//   }

}

// //============================================================================================
// // SOME SIMPLE TERMS
// //

// object Terms {

//   sealed trait U
//   case object T extends U

//   object OverUnit extends SliceOver[U] {

//     class UU extends IndexedType {

//       type TypeAt[J <: U] = U

//     }

//     val test0 : Sigma[UU, T.type] = Sigma[UU, T.type](T)
//     val test1 : IdSlice[UU, T.type] = unit(test0)

//     val test2 : Sigma[Id[IdSlice[UU, T.type]], T.type] = 
//       Sigma[Id[IdSlice[UU, T.type]], T.type](test1)
//     val test3 : IdSlice[UU, T.type] = fix(test2)

//     def encodeNat(n : Int) : IdSlice[UU, T.type] =
//       if (n <= 0) {
//         unit(Sigma[UU, T.type](T))
//       } else {
//         fix(Sigma[Id[IdSlice[UU, T.type]], T.type](encodeNat(n -1)))
//       }

//     // Great.  So we at least get natural numbers as expected.  Now we
//     // should also be able to generate lists from this setup.  How does
//     // that work???

//     // It's either a type A as an index or as the value over the unit ....

//     class Const[A] extends IndexedType {

//       type TypeAt[J <: U] = A

//     }

//     sealed trait LeafList[A] 
//     case class Leaf[A](a : A) extends LeafList[A]
//     case class Knot[A](ll : LeafList[A]) extends LeafList[A]

//     def encodeLeafList[A](ll : LeafList[A]) : IdSlice[Const[A], T.type] = 
//       ll match {
//         case Leaf(a) => unit(Sigma[Const[A], T.type](a))
//         case Knot(l) => fix(Sigma[Id[IdSlice[Const[A], T.type]], T.type](encodeLeafList(l)))
//       }

//     // So indeed, we get this free monad looking guy where data is stored at the leaves.
//     // By varying the indexing, or possibly adding an element constructor to the fix free
//     // monad constructor, I think we could also vary the internally stored data as well.

//     // Question: can these be decoded?

//     // def decodeLeafList[A](sl : Free[Id, Const[A], T.type]) : LeafList[A] = 
//     //   sl match {
//     //     case Return(el) => ???
//     //     case Free(el) => ???
//     //   }

//     // Basically, no.  You've still got some variance problems going through here.  Not quite
//     // sure how I'm going to fix that.

//   }


//   // Now what?  We could show that there is the evaluation function from this guy back down
//   // the the monad itself when we are given a monad.
 
// }
