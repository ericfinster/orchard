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

object Trees {

  case class Point[+A](a : A)

  //============================================================================================
  // RECURSORS
  //

  trait TreeRec extends NatRec1[Any] {
    type OnZero[+A] = Point[A]
    type OnSucc[P <: Nat, T[+_] <: Any, +A] = Slice[T, A]
  }

  trait CardinalRec extends NatRec1[Any] {
    type OnZero[+A] = Point[A]
    type OnSucc[P <: Nat, T[+_] <: Any, +A] = T[Tree[S[P], A]]
  }

  trait DerivativeRec extends NatRec1[Any] {
    type OnZero[+A] = Unit
    type OnSucc[P <: Nat, T[+_] <: Any, +A] = (Tree[P, Tree[S[P], A]], List[(A, T[Tree[S[P], A]])])
  }

  trait ContextRec extends NatRec1[Any] {
    type OnZero[+A] = Unit
    type OnSucc[P <: Nat, T[+_] <: Any, +A] = List[(A, Derivative[P, Tree[S[P], A]])]
  }

  trait DirectionRec extends NatRec0[Any] {
    type OnZero = Nothing
    type OnSucc[P <: Nat, T] = List[T]
  }

  //============================================================================================
  // TYPE DEFINITIONS
  //

  type Tree[N <: Nat, +A] = N#Rec1[Any, TreeRec, A] 
  type CardinalTree[N <: Nat, +A] = N#Rec1[Any, CardinalRec, A] 

  type Derivative[N <: Nat, +A] = N#Rec1[Any, DerivativeRec, A]
  type Context[N <: Nat, +A] = N#Rec1[Any, ContextRec, A]
  type Zipper[N <: Nat, +A] = (Tree[N, A], Context[N, A])

  type Direction[N <: Nat] = N#Rec0[Any, DirectionRec]
  type Address[N <: Nat] = Direction[S[N]]

  //============================================================================================
  // LOW DIMENSIONAL IMPLEMENTATIONS
  //

  type Tree0[+A] = Tree[_0, A] 
  type Tree1[+A] = Tree[_1, A] 
  type Tree2[+A] = Tree[_2, A] 
  type Tree3[+A] = Tree[_3, A] 
  type Tree4[+A] = Tree[_4, A]

  // type Card0[+A] = CardinalTree[_0, A]
  // type Card1[+A] = CardinalTree[_1, A]
  // type Card2[+A] = CardinalTree[_2, A]
  // type Card3[+A] = CardinalTree[_3, A]


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

  // trait IsConsTree[T, N <: Nat, G[_], A] {
  //   val dim : N
  //   def leibniz : T === Tree[N, G[A]]
  // }

  // trait IsDerivative[T, N <: Nat, A] {
  //   val dim : N
  //   def leibniz : T === Derivative[N, A]
  // }

  // trait IsContext[T, N <: Nat, A] {
  //   val dim : N
  //   def leibniz : T === Context[N, A]
  // }

  implicit def treesAreTrees[N <: Nat, A](implicit tfs : TreeFunctions[N]) : IsTree[Tree[N, A], N, A] = 
    new IsTree[Tree[N, A], N, A] {
      val tfns = tfs
      def leibniz : Tree[N, A] === Tree[N, A] = 
        refl[Tree[N, A]]
    }

  // implicit def consTreesAreConsTrees[N <: Nat, G[_], A](implicit n : N) : IsConsTree[Tree[N, G[A]], N, G, A] =
  //   new IsConsTree[Tree[N, G[A]], N, G, A] {
  //     val dim = n
  //     def leibniz : Tree[N, G[A]] === Tree[N, G[A]] =
  //       refl[Tree[N, G[A]]]
  //   }

  // implicit def contextsAreContexts[N <: Nat, A](implicit n : N) : IsContext[Context[N, A], N, A] =
  //   new IsContext[Context[N, A], N, A] {
  //     val dim = n
  //     def leibniz : Context[N, A] === Context[N, A] =
  //       refl[Context[N, A]]
  //   }

  // implicit def derivsAreDerivs[N <: Nat, A](implicit n : N) : IsDerivative[Derivative[N, A], N, A] =
  //   new IsDerivative[Derivative[N, A], N, A] {
  //     val dim = n
  //     def leibniz : Derivative[N, A] === Derivative[N, A] =
  //       refl[Derivative[N, A]]
  //   }

  // implicitly[IsTree[Tree[_0, Int], _0, Int]]
  // implicitly[IsTree[Tree[_1, Int], _1, Int]]
  // implicitly[IsTree[Tree[_2, Int], _2, Int]]
  // implicitly[IsTree[Tree[_3, Int], _3, Int]]

  // implicitly[IsConsTree[Tree[_3, List[Int]], _3, List, Int]]

  // implicitly[IsDerivative[Derivative[_3, Int], _3, Int]]

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

    def flatten(implicit hasPred : HasPred[N]) : Option[Tree[hasPred.P, Unit]] = {
      val sfns = tfns.asInstanceOf[TreeSuccFunctions[hasPred.P]]
      val pfns = sfns.prev
      val test = tree.asInstanceOf[Tree[S[hasPred.P], A]]
      val last = pfns.flatten(test)
      last
    }

  }

  // implicit class SuccOps[T, N <: Nat, A](t : T)(implicit val isTree : IsTree[T, S[N], A]) {

  //   import isTree._

  //   val tree : Tree[S[N], A] =
  //     subst(t)(leibniz)

  //   def flatten : Option[Tree[N, Unit]] =
  //     TreeLib.flatten[N, A](dim.pred, tree)

  // }

  // implicit class DerivativeOps[D, N <: Nat, A](d : D)(implicit val isDerivative : IsDerivative[D, N, A]) {

  //   import isDerivative._

  //   val deriv : Derivative[N, A] =
  //     subst(d)(leibniz)

  //   def plugWith(a : A) : Tree[N, A] = 
  //     TreeLib.plug(dim, deriv, a)

  // }

  //============================================================================================
  // CONSTRUCTORS AND EXTRACTORS
  //

  object Leaf {

    def apply[N <: Nat]() : Tree[S[N], Nothing] = {
      type P[+X] = Tree[N, X]
      Cap[P]()
    }


    def unapply[N <: Nat, A](s : Tree[S[N], A]) : Boolean = {
      type P[+X] = Tree[N, X]
      (s : Slice[P, A]) match {
        case Cap() => true
        case _ => false
      }
    }

  }

  object Branch {

    def apply[N <: Nat, A](a : A, shell : Tree[N, Tree[S[N], A]]) : Tree[S[N], A] = {
      type P[+X] = Tree[N, X]
      Joint[P, A](a, shell)
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
