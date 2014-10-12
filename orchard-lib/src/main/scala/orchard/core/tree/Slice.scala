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

// I think the idea is that we need to carry some kind of tag along with the slice
// construction so that we can have operations classes.

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

