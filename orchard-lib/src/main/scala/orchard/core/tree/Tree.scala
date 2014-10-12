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

  implicit def treeIsTraverse[N <: Nat](implicit n : N) : Traverse[N#Tree] = 
    new Traverse[N#Tree] {

      override def map[A, B](tr : Tree[N, A])(f : A => B) : Tree[N, B] = ???
        // tfns.map(tr, f)

      def traverseImpl[G[_], A, B](tr : Tree[N, A])(f : A => G[B])(implicit isA : Applicative[G]) : G[Tree[N, B]] = ???
        // tfns.traverse(tr, f)

    }

  implicit def treeFnsFromNat[N <: Nat](implicit n : N) : TreeFunctions[N] = 
    n match {
      case IsZero(zm) => zm.zeroCoe.subst[TreeFunctions](TreeZeroFunctions)
      case IsSucc(sm) => sm.succCoe.subst[TreeFunctions](treeFnsFromNat[sm.P](sm.p).succ)
    }

  //============================================================================================
  // OPERATIONS CLASSES
  //

  trait TreeType[T] {

    type A
    type N <: Nat

    implicit val dim : N

    def leibniz : T === Tree[N, A]

  }

  implicit def treeIsTreeType[N0 <: Nat, A0](implicit n0 : N0) : TreeType[Tree[N0, A0]] = 
    new TreeType[Tree[N0, A0]] {

      type A = A0
      type N = N0

      implicit val dim : N = n0

      def leibniz : Tree[N, A] === Tree[N, A] = refl[Tree[N, A]]

    }

  implicit class TreeOps[T](t : T)(implicit val tt : TreeType[T]) {

    import tt._

    val tree : Tree[N, A] = subst(t)(leibniz)
    val tfns : TreeFunctions[N] = implicitly[TreeFunctions[N]]

    def opsTest : Unit = ()

    def seek(addr : Address[N]) : Option[Zipper[N, A]] = 
      tfns.seek(addr, (tree, tfns.emptyContext))

    def zipComplete[B](other : Tree[N, B]) : Option[Tree[N, (A, B)]] = 
      tfns.zipComplete(tree, other)

    def zipWithAddress : Tree[N, (A, Address[N])] =
      tfns.zipWithAddress(tree)

    def rootValue : Option[A] = 
      tfns.value(tree)

    // def nodes : List[A] = 
    //   treeIsTraverse[N].toList(tree)

  }

  // The reason this does not work is that implicit classes can only have a single type parameter
  // according to the spec!
  // implicit class TreeOps[N <: Nat, +A](tree : Tree[N, A])(implicit tfns : TreeFunctions[N]) {

  //   def opsTest : Unit = ()

  //   def seek(addr : Address[N]) : Option[Zipper[N, A]] = 
  //     tfns.seek(addr, (tree, tfns.emptyContext))

  //   def zipComplete[B](other : Tree[N, B]) : Option[Tree[N, (A, B)]] = 
  //     tfns.zipComplete(tree, other)

  //   def zipWithAddress : Tree[N, (A, Address[N])] =
  //     tfns.zipWithAddress(tree)

  //   def rootValue : Option[A] = 
  //     tfns.value(tree)

  //   def nodes : List[A] = 
  //     treeIsTraverse[N].toList(tree)

  // }

  // implicit class DerivativeOps[N <: Nat, +A](deriv : Derivative[N, A])(implicit tfns : TreeFunctions[N]) {

  //   def plugWith[B >: A](b : B) : Tree[N, B] = 
  //     tfns.plug(deriv, b)

  // }

}

trait TypeTests {

  import Trees._

  type Tree0[+A] = Point[A]
  type Tree1[+A] = Slice[Tree0, Address[_0], A]
  type Tree2[+A] = Slice[Tree1, Address[_1], A]
  type Tree3[+A] = Slice[Tree2, Address[_2], A]
  type Tree4[+A] = Slice[Tree3, Address[_3], A]

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
