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
import scalaz.Id._
import scalaz.Leibniz._
import scalaz.std.option._

import Nats._
import Slice._

trait Trees {

  import Nats._

  //============================================================================================
  // RECURSORS
  //

  trait TreeRec extends NatRec1[Any] {
    type OnZero[+A] = Id[A]
    type OnSucc[P <: Nat, T[+_] <: Any, +A] = Slice[T, A]
  }

  trait CardinalRec extends NatRec1[Any] {
    type OnZero[+A] = Id[A]
    type OnSucc[P <: Nat, T[+_] <: Any, +A] = T[S[P]#Rec1[Any, TreeRec, A]]
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

  def cap[N <: Nat, A] : Tree[S[N], A] = 
    Cap[({ type L[+X] = Tree[N, X] })#L, A]()

  def joint[N <: Nat, A](a : A, shell : Tree[N, Tree[S[N], A]]) : Tree[S[N], A] = 
    Joint[({ type L[+X] = Tree[N, X] })#L, A](a, shell)

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

  def nil[A] : Tree1[A] = Cap()

  def cons[A](a : A, t : Tree1[A]) : Tree1[A] = 
    Joint[Id, A](a, t)

  def leaf[A] : Tree2[A] = Cap[Tree1, A]()

  def node[A](a : A, brs : Tree1[Tree2[A]]) : Tree2[A] =
    Joint[Tree1, A](a, brs)

  //============================================================================================
  // WITNESS TYPE CLASSES
  //

  trait IsTree[T, A] { 

    type Dim <: Nat 
    val dim : Dim 

    def leibniz : T === Tree[Dim, A]

  }

  implicit def treesAreTrees[N <: Nat, A](implicit n : N) : IsTree[Tree[N, A], A] = 
    new IsTree[Tree[N, A], A] {

      type Dim = N
      val dim = n

      def leibniz : Tree[N, A] === Tree[Dim, A] = 
        refl[Tree[N, A]]

    }

  implicitly[IsTree[Tree[_0, Int], Int]]
  implicitly[IsTree[Tree[_1, Int], Int]]
  implicitly[IsTree[Tree[_2, Int], Int]]
  implicitly[IsTree[Tree[_3, Int], Int]]

  //============================================================================================
  // OPERATIONS CLASSES
  //

  implicit class TreeOps[T, A](t : T)(implicit val isTree : IsTree[T, A]) {

    import isTree._

    val tree : Tree[Dim, A] = 
      subst(t)(leibniz)

    def hello : Unit = ()

    def map[B](f : A => B) : Tree[Dim, B] = ???

    def zipComplete[B](other : Tree[Dim, B]) : Option[Tree[Dim, (A, B)]] = 
      TreeLib.zipComplete[Dim, A, B](dim, tree, other)

  }

  //============================================================================================
  // DIMENSION MATCHING
  //

  object ZeroDim {
  }

  object OneDim {
  }

  object SuccDim {
  }

  //============================================================================================
  // TREE FUNCTIONS
  //

  object TreeLib {

    implicit def treeCoh[N <: Nat, M <: Nat, A](t : Tree[N, A])(implicit eq : N === M) : Tree[M, A] = ???
    implicit def treeCoe[N <: Nat, M <: Nat, A](t : Tree[M, A])(implicit eq : N === M) : Tree[N, A] = ???

    implicit def derivCoh[N <: Nat, M <: Nat, A](d : Derivative[N, A])(implicit eq : N === M) : Derivative[M, A] = ???
    implicit def derivCoe[N <: Nat, M <: Nat, A](d : Derivative[M, A])(implicit eq : N === M) : Derivative[N, A] = ???

    implicit def cntxtCoh[N <: Nat, M <: Nat, A](d : Context[N, A])(implicit eq : N === M) : Context[M, A] = ???
    implicit def cntxtCoe[N <: Nat, M <: Nat, A](d : Context[M, A])(implicit eq : N === M) : Context[N, A] = ???

    def plug[N <: Nat, A](n : N, d : Derivative[N, A], a : A) : Tree[N, A] = 
      n match {
        case IsZero(zm) => { import zm._ ; a : Tree[_0, A] }
        case IsSucc(sm) => { import sm._ ;
          (d : Derivative[S[P], A]) match {
            case (shell, context) => 
              close[S[P], A](S(p), context, joint(a, shell))
          }
        }
      }

    def close[N <: Nat, A](n : N, c : Context[N, A], t : Tree[N, A]) : Tree[N, A] =
      n match {
        case IsZero(zm) => { import zm._ ; t }
        case IsSucc(sm) => { import sm._ ;
          (c : Context[S[P], A]) match {
            case Nil => t
            case (a , d) :: cs => {
              close[S[P], A](S(p), cs, joint(a , plug[P, Tree[S[P], A]](p, d, t)))
            }
          }
        }
      }


    def zipComplete[N <: Nat, A, B](n : N, ta : Tree[N, A], tb : Tree[N, B]) : Option[Tree[N, (A, B)]] = 
      n match {
        case IsZero(zm) => { import zm._ ;

          val a : Tree[_0, A] = ta
          val b : Tree[_0, B] = tb
          val ab : Tree[_0, (A, B)] = (a, b)

          Some(ab)

        }
        case IsSucc(sm) => { import sm._ ;

          val tra : Tree[S[P], A] = ta
          val trb : Tree[S[P], B] = tb

          // tra match {
          //   case IsCap() => ()
          //   case IsJoint(a, shell) => ()
          // }

          // (tra, trb) match {
          //   case (Cap(), Cap()) => ??? //Some(Cap())
      //       case (Joint(a, ash), Joint(b, bsh)) => {

      //         for {
      //           branchPairs <- zipComplete[P, STree[A], STree[B]](p, ash, bsh)
      //           zippedShell <- sequenceT(p,
      //             (mapT[P, (STree[A], STree[B]), Option[STree[(A, B)]]](p, branchPairs, {
      //               case (t1 : STree[A], t2 : STree[B]) => zipComplete[ST[P], A, B](ST(p), t1, t2)
      //             }))
      //           )
      //         } yield Joint((a, b), zippedShell)

      //       }
          //   case (_, _) => None
          // }

          ???

        }
      }


  }

  //============================================================================================
  // TESTS AND EXPERIMENTS
  //

  // val test : Tree[_2, Int] = ???

  // type PT[+A] = Tree[_1, A] 

  // val temp : Slice[PT, Int] = test

  // temp match {
  //   case Cap() => ???
  //   // case Joint(a, shell) => ???
  //   case _ => ???
  // }

}

trait TreeTests extends Trees {

  // type Card0[+A] = Tree0[A]
  // type Card1[+A] = Tree0[Tree1[A]]
  // type Card2[+A] = Tree0[Tree1[Tree2[A]]]
  // type Card3[+A] = Tree0[Tree1[Tree2[Tree3[A]]]]

  // type D0[+A] = Unit
  // type C0[+A] = Unit

  // type D1[+A] = (Tree0[Tree1[A]], List[(A, D0[Tree1[A]])])
  // type C1[+A] = List[(A, D0[Tree1[A]])]

  // type D2[+A] = (Tree1[Tree2[A]], List[(A, D1[Tree2[A]])])
  // type C2[+A] = List[(A, D1[Tree2[A]])]

  implicitly[CardinalTree[_0, Int] =:= Tree0[Int]]
  implicitly[CardinalTree[_1, Int] =:= Tree0[Tree1[Int]]]
  implicitly[CardinalTree[_2, Int] =:= Tree0[Tree1[Tree2[Int]]]]
  implicitly[CardinalTree[_3, Int] =:= Tree0[Tree1[Tree2[Tree3[Int]]]]]

  // implicitly[Derivative[_0, Int] =:= D0[Int]]
  // implicitly[Derivative[_1, Int] =:= D1[Int]]

  // implicitly[Direction[_0] =:= Nothing]
  // implicitly[Direction[_1] =:= List[Nothing]]
  // implicitly[Direction[_2] =:= List[List[Nothing]]]

}
