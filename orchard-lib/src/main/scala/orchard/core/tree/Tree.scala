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

import Slice._

trait TreeRecs {

  trait TreeRec extends NatConsRec[Any] {
    type OnZero[+A] = Id[A]
    type OnSucc[P <: Nat, T[+_] <: Any, +A] = Slice[T, A]
  }

  trait CardinalRec extends NatConsRec[Any] {
    type OnZero[+A] = Id[A]
    type OnSucc[P <: Nat, T[+_] <: Any, +A] = T[S[P]#ConsRec[Any, TreeRec, A]]
  }

  trait DerivativeRec extends NatConsRec[Any] {
    type OnZero[+A] = Unit
    type OnSucc[P <: Nat, T[+_] <: Any, +A] = (Tree[P, Tree[S[P], A]], List[(A, T[Tree[S[P], A]])])
  }

  trait ContextRec extends NatConsRec[Any] {
    type OnZero[+A] = Unit
    type OnSucc[P <: Nat, T[+_] <: Any, +A] = List[(A, Derivative[P, Tree[S[P], A]])]
  }

  trait DirectionRec extends NatRec[Any] {
    type OnZero = Nothing
    type OnSucc[P <: Nat, T] = List[T]
  }

  type Tree[N <: Nat, +A] = N#ConsRec[Any, TreeRec, A] 
  type CardinalTree[N <: Nat, +A] = N#ConsRec[Any, CardinalRec, A] 

  type Derivative[N <: Nat, +A] = N#ConsRec[Any, DerivativeRec, A]
  type Context[N <: Nat, +A] = N#ConsRec[Any, ContextRec, A]
  type Zipper[N <: Nat, +A] = (Tree[N, A], Context[N, A])

  type Direction[N <: Nat] = N#Rec[Any, DirectionRec]
  type Address[N <: Nat] = Direction[S[N]]

  import Nats._

  type Tree0[+A] = Tree[__0, A] 
  type Tree1[+A] = Tree[__1, A] 
  type Tree2[+A] = Tree[__2, A] 
  type Tree3[+A] = Tree[__3, A] 

  def nil[A] : Tree1[A] = Cap()

  def cons[A](a : A, t : Tree1[A]) : Tree1[A] = 
    Joint[Id, A](a, t)

  def leaf[A] : Tree2[A] = Cap[Tree1, A]()

  def node[A](a : A, brs : Tree1[Tree2[A]]) : Tree2[A] =
    Joint[Tree1, A](a, brs)

  def cap[N <: Nat, A] : Tree[S[N], A] = 
    Cap[({ type L[+X] = Tree[N, X] })#L, A]()

  def joint[N <: Nat, A](a : A, shell : Tree[N, Tree[S[N], A]]) : Tree[S[N], A] = 
    Joint[({ type L[+X] = Tree[N, X] })#L, A](a, shell)

  // object IsCap {

  //   def unapply[N <: Nat, A](t : Tree[S[N], A]) : Boolean = ???

  // }

  // object IsJoint {

  //   def unapply[N <: Nat, A](t : Tree[S[N], A]) : Option[(A, Tree[N, Tree[S[N], A]])] =
  //     t match {
  //       case Joint(a, shell) => None
  //       case _ => None
  //     }

  // }

  val test : Tree[__2, Int] = ???

  val temp : Slice[({ type L[+X] = Tree[__1, X] })#L, Int] = test

  // test match {
  //   case Cap() => ???
  //   case Joint(a, shell) => ???
  //   case _ => ???
  // }

}

trait TreeStuff extends TreeRecs {

  import Nats._

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

  implicitly[IsTree[Tree[__0, Int], Int]]
  implicitly[IsTree[Tree[__1, Int], Int]]
  implicitly[IsTree[Tree[__2, Int], Int]]
  implicitly[IsTree[Tree[__3, Int], Int]]

  implicit class TreeOps[T, A](t : T)(implicit val isTree : IsTree[T, A]) {

    import isTree._

    val tree : Tree[Dim, A] = 
      subst(t)(leibniz)

    def hello : Unit = ()

    def map[B](f : A => B) : Tree[Dim, B] = ???

    def zipComplete[B](other : Tree[Dim, B]) : Option[Tree[Dim, (A, B)]] = 
      TreeLib.zipComplete[Dim, A, B](dim, tree, other)

  }

  object TreeLib {

    implicit def treeCoh[N <: Nat, M <: Nat, A](t : Tree[N, A])(implicit eq : N === M) : Tree[M, A] = ???
    implicit def treeCoe[N <: Nat, M <: Nat, A](t : Tree[M, A])(implicit eq : N === M) : Tree[N, A] = ???

    implicit def derivCoh[N <: Nat, M <: Nat, A](d : Derivative[N, A])(implicit eq : N === M) : Derivative[M, A] = ???
    implicit def derivCoe[N <: Nat, M <: Nat, A](d : Derivative[M, A])(implicit eq : N === M) : Derivative[N, A] = ???

    implicit def cntxtCoh[N <: Nat, M <: Nat, A](d : Context[N, A])(implicit eq : N === M) : Context[M, A] = ???
    implicit def cntxtCoe[N <: Nat, M <: Nat, A](d : Context[M, A])(implicit eq : N === M) : Context[N, A] = ???

    def plug[N <: Nat, A](n : N, d : Derivative[N, A], a : A) : Tree[N, A] = 
      n match {
        case IsZero(zm) => { import zm._ ; a : Tree[__0, A] }
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

          val a : Tree[__0, A] = ta
          val b : Tree[__0, B] = tb
          val ab : Tree[__0, (A, B)] = (a, b)

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

}

object Trees extends TreeRecs with TreeFunctions {

  // type TreeE[+A] = Unit
  // type Tree0[+A] = Id[A]
  // type Tree1[+A] = Slice[Tree0, A]
  // type Tree2[+A] = Slice[Tree1, A]
  // type Tree3[+A] = Slice[Tree2, A]

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

  // import Nats._

  // implicitly[Tree[__0, Int] =:= Tree0[Int]]
  // implicitly[Tree[__1, Int] =:= Tree1[Int]]
  // implicitly[Tree[__2, Int] =:= Tree2[Int]]
  // implicitly[Tree[__3, Int] =:= Tree3[Int]]

  // implicitly[CardinalTree[__0, Int] =:= Tree0[Int]]
  // implicitly[CardinalTree[__1, Int] =:= Tree0[Tree1[Int]]]
  // implicitly[CardinalTree[__2, Int] =:= Tree0[Tree1[Tree2[Int]]]]
  // implicitly[CardinalTree[__3, Int] =:= Tree0[Tree1[Tree2[Tree3[Int]]]]]

  // implicitly[Derivative[__0, Int] =:= D0[Int]]
  // implicitly[Derivative[__1, Int] =:= D1[Int]]

  // implicitly[Direction[__0] =:= Nothing]
  // implicitly[Direction[__1] =:= List[Nothing]]
  // implicitly[Direction[__2] =:= List[List[Nothing]]]

  sealed abstract class TreeType[T[+_]] { val idx : TreeIndex }

  sealed trait ZeroType[T[+_]] extends TreeType[T]
  sealed trait SuccType[T[+_], P[+_]] extends TreeType[T] { val p : TreeIndex }

  implicit def idIsTree : ZeroType[Id] = new ZeroType[Id] { val idx = ZT }

  implicit def sliceIsTree[P[+_]](implicit isTree : TreeType[P]) : SuccType[({ type L[+X] = Slice[P, X] })#L, P] = 
    new SuccType[({ type L[+X] = Slice[P, X] })#L, P] { val p = isTree.idx ; val idx = ST(isTree.idx) }

  implicit class SuccTreeOps[T[+_], P[+_], A](tree : T[A])(implicit val succTree : SuccType[T, P]) {

    import succTree._

    def flatten : Option[P[Unit]] = {
      Trees.flatten[p.Self, A](
        p.asInstanceOf[p.Self],
        tree.asInstanceOf[Slice[p.Self#Tree, A]]
      ).asInstanceOf[Option[P[Unit]]]
    }

  }

}
