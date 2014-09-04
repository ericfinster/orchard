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

  // trait CardinalRec extends NatRec1[Any] {
  //   type OnZero[+A] = Id[A]
  //   type OnSucc[P <: Nat, T[+_] <: Any, +A] = T[Tree[S[P], A]]
  // }

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
  // type CardinalTree[N <: Nat, +A] = N#Rec1[Any, CardinalRec, A] 

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

  trait IsTree[T, N <: Nat, A] { 

    val dim : N

    def leibniz : T === Tree[N, A]

  }

  implicit def treesAreTrees[N <: Nat, A](implicit n : N) : IsTree[Tree[N, A], N, A] = 
    new IsTree[Tree[N, A], N, A] {

      val dim = n

      def leibniz : Tree[N, A] === Tree[N, A] = 
        refl[Tree[N, A]]

    }

  implicitly[IsTree[Tree[_0, Int], _0, Int]]
  implicitly[IsTree[Tree[_1, Int], _1, Int]]
  implicitly[IsTree[Tree[_2, Int], _2, Int]]
  implicitly[IsTree[Tree[_3, Int], _3, Int]]

  //============================================================================================
  // OPERATIONS CLASSES
  //

  implicit class TreeOps[T, N <: Nat, A](t : T)(implicit val isTree : IsTree[T, N, A]) {

    import isTree._

    val tree : Tree[N, A] = 
      subst(t)(leibniz)

    def map[B](f : A => B) : Tree[N, B] = ???

    def zipComplete[B](other : Tree[N, B]) : Option[Tree[N, (A, B)]] = 
      TreeLib.zipComplete[N, A, B](dim, tree, other)

  }

  //============================================================================================
  // DIMENSION MATCHING
  //

  trait ZeroDimMatch[N <: Nat] extends ZeroMatch[N]
  trait OneDimMatch[N <: Nat] extends OneMatch[N]

  trait SuccDimMatch[N <: Nat] extends SuccMatch[N] {

    type PF[+A] = Tree[P, A]

  // type PT[+A] = Tree[_1, A] 

  }

  trait DblSuccDimMatch[N <: Nat] extends DblSuccMatch[N]

  object IsZeroDim {

    def unapply[N <: Nat](n : N) : Option[ZeroDimMatch[N]] = 
      n match {
        case IsZero(zm) => Some(
          new ZeroDimMatch[N] {
            implicit def zeroCoh : N === _0 = zm.zeroCoh
            implicit def zeroCoe : _0 === N = zm.zeroCoe
          }
        )
        case _ => None
      }

  }

  object IsOneDim {

    def unapply[N <: Nat](n : N) : Option[OneDimMatch[N]] =
      n match {
        case IsOne(om) => Some(
          new OneDimMatch[N] {
            implicit def oneCoh : N === _1 = om.oneCoh
            implicit def oneCoe : _1 === N = om.oneCoe
          }
        )
        case _ => None
      }

  }

  object IsSuccDim {

    def unapply[N <: Nat](n : N) : Option[SuccDimMatch[N]] = 
      n match {
        case IsSucc(sm) => Some(
          new SuccDimMatch[N] {

            type P = sm.P

            implicit val p : P = sm.p

            implicit def succCoh : N === S[P] = sm.succCoh
            implicit def succCoe : S[P] === N = sm.succCoe

          }
        )
        case _ => None
      }

  }

  object IsDblSuccDim {

    def unapply[N <: Nat](n : N) : Option[DblSuccDimMatch[N]] = 
      n match {
        case IsDblSucc(dm) => Some(
          new DblSuccDimMatch[N] {

            type PP = dm.PP
            
            implicit val pp : PP = dm.pp

            implicit def dblSuccCoh : N === S[S[PP]] = dm.dblSuccCoh
            implicit def dblSuccCoe : S[S[PP]] === N = dm.dblSuccCoe

          }
        )
        case _ => None
      }

  }

  //============================================================================================
  // DIMENSION CONVERSIONS
  //

  def rewrite[F[_], A, B](fa : F[A])(implicit eq : A === B) : F[B] = eq.subst[F](fa)

  // Implicit Equalities
  implicit def liftTree[M <: Nat, N <: Nat, A](implicit eq : M === N) : Tree[M, A] === Tree[N, A] =
    force[Nothing, Any, Tree[M, A], Tree[N, A]]

  implicit def liftDerv[M <: Nat, N <: Nat, A](implicit eq : M === N) : Derivative[M, A] === Derivative[N, A] = 
    force[Nothing, Any, Derivative[M, A], Derivative[N, A]]

  implicit def liftCntxt[M <: Nat, N <: Nat, A](implicit eq : M === N) : Context[M, A] === Context[N, A] = 
    force[Nothing, Any, Context[M, A], Context[N, A]]

  implicit def liftZipper[M <: Nat, N <: Nat, A](implicit eq : M === N) : Zipper[M, A] === Zipper[N, A] = 
    force[Nothing, Any, Zipper[M, A], Zipper[N, A]]

  implicit def liftDir[M <: Nat, N <: Nat](implicit eq : M === N) : Direction[M] === Direction[N] =
    force[Nothing, Any, Direction[M], Direction[N]]

  // Implicit Conversions
  implicit def treeCoerce[M <: Nat, N <: Nat, A](t : Tree[M, A])(implicit eq : M === N) : Tree[N, A] =
    subst(t)(implicitly[Tree[M, A] === Tree[N, A]])

  implicit def derivCoerce[M <: Nat, N <: Nat, A](d : Derivative[M, A])(implicit eq :M === N) : Derivative[N, A] = 
    subst(d)(implicitly[Derivative[M, A] === Derivative[N, A]])

  implicit def contextCoerce[M <: Nat, N <: Nat, A](c : Context[M, A])(implicit eq : M === N) : Context[N, A] = 
    subst(c)(implicitly[Context[M, A] === Context[N, A]])

  implicit def zipperCoerce[M <: Nat, N <: Nat, A](z : Zipper[M, A])(implicit eq : M === N) : Zipper[N, A] = 
    subst(z)(implicitly[Zipper[M, A] === Zipper[N, A]])

  implicit def dirCoerce[M <: Nat, N <: Nat](d : Direction[M])(implicit eq : M === N) : Direction[N] =
    subst(d)(implicitly[Direction[M] === Direction[N]])

  //============================================================================================
  // TREE FUNCTIONS
  //

  object TreeLib {

    def plug[N <: Nat, A](n : N, d : Derivative[N, A], a : A) : Tree[N, A] = 
      n match {
        case IsZeroDim(zm) => { import zm._ ; a : Tree[_0, A] }
        case IsSuccDim(sm) => { import sm._ ;
          (d : Derivative[S[P], A]) match {
            case (shell, context) => 
              close[S[P], A](S(p), context, joint(a, shell))
          }
        }
      }

    def close[N <: Nat, A](n : N, c : Context[N, A], t : Tree[N, A]) : Tree[N, A] =
      n match {
        case IsZeroDim(zm) => { import zm._ ; t }
        case IsSuccDim(sm) => { import sm._ ;
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
        case IsZeroDim(zm) => { import zm._ ;

          val a : Tree[_0, A] = ta
          val b : Tree[_0, B] = tb
          val ab : Tree[_0, (A, B)] = (a, b)

          Some(ab)

        }
        case IsSuccDim(sm) => { import sm._ ;

          val tra : Tree[S[P], A] = ta
          val trb : Tree[S[P], B] = tb

          (tra : Slice[PF, A], trb : Slice[PF, B]) match {
            case (Cap(), Cap()) => {
              val capAB : Tree[S[P], (A, B)] = Cap[PF, (A, B)]() 
              Some(capAB)
            }
            case (Joint(a, ash), Joint(b, bsh)) => {

              for {
                branchPairs <- ash.zipComplete(bsh)
              //   zippedShell <- sequenceT(p,
              //     (mapT[P, (Tree[S[P], A], Tree[S[P], B]), Option[Tree[S[P], (A, B)]]](p, branchPairs, {
              //       case (t1 : Tree[S[P], A], t2 : STree[S[P], B]) => zipComplete[S[P], A, B](S(p), t1, t2)
              //     }))
              //   )
              } yield ??? //Joint((a, b), zippedShell)
            }
            case (_, _) => None
          }
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

  // implicitly[CardinalTree[_0, Int] =:= Tree0[Int]]
  // implicitly[CardinalTree[_1, Int] =:= Tree0[Int]]
  // implicitly[CardinalTree[_2, Int] =:= Tree0[Tree1[Int]]]
  // implicitly[CardinalTree[_3, Int] =:= Tree0[Tree1[Tree2[Int]]]]
  // implicitly[CardinalTree[_4, Int] =:= Tree0[Tree1[Tree2[Tree3[Int]]]]]

  // implicitly[Derivative[_0, Int] =:= D0[Int]]
  // implicitly[Derivative[_1, Int] =:= D1[Int]]

  // implicitly[Direction[_0] =:= Nothing]
  // implicitly[Direction[_1] =:= List[Nothing]]
  // implicitly[Direction[_2] =:= List[List[Nothing]]]

}
