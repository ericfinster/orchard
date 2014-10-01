/**
  * Tree.scala - Higher Dimensional Trees
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import scala.language.higherKinds
import scala.language.implicitConversions

import scalaz._
import scalaz.Leibniz._
import scalaz.std.option._

import Nats._
import Slice._

case class Point[+A](a : A)

object Trees {

  //============================================================================================
  // TYPE DEFINITIONS
  //

  type Tree[N <: Nat, +A] = N#Tree[A]
  type Cardinal[N <: Nat, +A] = N#Cardinal[A]

  type Context[N <: Nat, +A] = N#Context[A]
  type Derivative[N <: Nat, +A] = N#Derivative[A]
  type Zipper[N <: Nat, +A] = (Tree[N, A], Context[N, A])

  type Direction[N <: Nat] = N#Direction
  type Address[N <: Nat] = List[N#Direction]

  //============================================================================================
  // FUNCTION IMPLICITS
  //

  implicit def haveZeroFunctions : TreeFunctions[_0] = TreeZeroFunctions

  implicit def haveSuccFunctions[N <: Nat](implicit prev : TreeFunctions[N]) : TreeFunctions[S[N]] =
    prev match {
      case TreeZeroFunctions => TreeOneFunctions.asInstanceOf[TreeFunctions[S[N]]]
      case TreeOneFunctions => TreeDblSuccFunctions(TreeZeroFunctions).asInstanceOf[TreeFunctions[S[N]]]
      case TreeDblSuccFunctions(pp) => TreeDblSuccFunctions(haveSuccFunctions(pp)).asInstanceOf[TreeFunctions[S[N]]]
    }

  //============================================================================================
  // WITNESS TYPE CLASSES
  //

  // Actually, these all look like unapplies .... can you use that?

  trait IsTree[T, N <: Nat, A] { 
    val tfns : TreeFunctions[N]
    def leibniz : T === Tree[N, A]
  }

  implicit def treesAreTrees[N <: Nat, A](implicit tfs : TreeFunctions[N]) : IsTree[Tree[N, A], N, A] = 
    new IsTree[Tree[N, A], N, A] {
      val tfns = tfs
      def leibniz : Tree[N, A] === Tree[N, A] = 
        refl[Tree[N, A]]
    }

  //============================================================================================
  // OPERATIONS CLASSES
  //

  implicit class TreeOps[T, N <: Nat, A](t : T)(implicit val isTree : IsTree[T, N, A]) {

    import isTree._

    val tree : Tree[N, A] = 
      subst(t)(leibniz)

    def map[B](f : A => B) : Tree[N, B] = 
      tfns.map(tree, f)

    def traverse[G[_], B](f : A => G[B])(implicit apG : Applicative[G]) : G[Tree[N, B]] =
      tfns.traverse(tree, f)

    def seek(addr : Address[N]) : Option[Zipper[N, A]] = 
      tfns.seek(addr, (tree, tfns.emptyContext))

    def constantWith[B](b : B) : Tree[N, B] = 
      tfns.const(b, tree)

    def zipComplete[B](other : Tree[N, B]) : Option[Tree[N, (A, B)]] = 
      tfns.zipComplete(tree, other)

    def zipWithCorolla : Tree[N, (A, Derivative[N, A])] = 
      tfns.zipWithCorolla(tree)

    def zipWithAddress : Tree[N, (A, Address[N])] =
      tfns.zipWithAddress(tree)

    // I think sequence and traverse could be handled with an unapply

    def flatten(implicit isSucc : IsSucc[N]) : Option[Tree[isSucc.P, Unit]] = {
      val sfns = tfns.asInstanceOf[TreeSuccFunctions[isSucc.P]]
      val pfns = sfns.prev
      val test = tree.asInstanceOf[Tree[S[isSucc.P], A]]
      val last = pfns.flatten(test)
      last
    }

  }

  //============================================================================================
  // CONSTRUCTORS AND EXTRACTORS
  //

  object Pt {

    def apply[A](a : A) : Tree[_0, A] = Point(a)

  }

  object Leaf {

    def apply[N <: Nat](implicit isSucc : IsSucc[N]) : Tree[N, Nothing] = {
      type P[+X] = Tree[isSucc.P, X]
      isSucc.leibniz.subst[({ type L[N <: Nat] = Tree[N, Nothing] })#L](Cap[P]())
    }

    def unapply[N <: Nat, A](s : Tree[S[N], A]) : Boolean = {
      type P[+X] = Tree[N, X]
      (s : Slice[P, A]) match {
        case Cap() => true
        case _ => false
      }
    }

  }

  object Node {

    def apply[N <: Nat, A, B <: A](b : B, shell : Tree[N, Tree[S[N], A]]) : Tree[S[N], A] = {
      type P[+X] = Tree[N, X]
      Joint[P, A](b, shell)
    }

    def unapply[N <: Nat, A](s : Tree[S[N], A]) : Option[(A, Tree[N, Tree[S[N], A]])] = {
      type P[+X] = Tree[N, X]
      (s : Slice[P, A]) match {
        case Joint(a, shell) => Some((a, shell))
        case _ => None
      }
    }

  }

}

trait TypeTests {

  import Trees._

  type Tree0[+A] = Point[A]
  type Tree1[+A] = Slice[Tree0, A]
  type Tree2[+A] = Slice[Tree1, A]
  type Tree3[+A] = Slice[Tree2, A]
  type Tree4[+A] = Slice[Tree3, A]

  type Card0[+A] = Point[A]
  type Card1[+A] = Card0[Tree1[A]]
  type Card2[+A] = Card1[Tree2[A]]
  type Card3[+A] = Card2[Tree3[A]]
  type Card4[+A] = Card3[Tree4[A]]

  implicitly[Tree0[Int] =:= Tree[_0, Int]]
  implicitly[Tree1[Int] =:= Tree[_1, Int]]
  implicitly[Tree2[Int] =:= Tree[_2, Int]]
  implicitly[Tree3[Int] =:= Tree[_3, Int]]
  implicitly[Tree4[Int] =:= Tree[_4, Int]]

  implicitly[Card0[Int] =:= Cardinal[_0, Int]]
  implicitly[Card1[Int] =:= Cardinal[_1, Int]]
  implicitly[Card2[Int] =:= Cardinal[_2, Int]]
  implicitly[Card3[Int] =:= Cardinal[_3, Int]]
  implicitly[Card4[Int] =:= Cardinal[_4, Int]]

  type CC0[+A] = Cardinal[_0, A]
  type CC1[+A] = Cardinal[_1, A]
  type CC2[+A] = Cardinal[_2, A]
  type CC3[+A] = Cardinal[_3, A]
  type CC4[+A] = Cardinal[_4, A]

  implicitly[CC0[Int] =:= Tree0[Int]]
  implicitly[CC1[Int] =:= Tree0[Tree1[Int]]]
  implicitly[CC2[Int] =:= Tree0[Tree1[Tree2[Int]]]]
  implicitly[CC3[Int] =:= Tree0[Tree1[Tree2[Tree3[Int]]]]]
  implicitly[CC4[Int] =:= Tree0[Tree1[Tree2[Tree3[Tree4[Int]]]]]]

}
