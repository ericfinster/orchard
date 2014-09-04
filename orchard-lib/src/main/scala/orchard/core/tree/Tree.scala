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

  // Actually, these all look like unapplies .... can you use that?

  trait IsTree[T, N <: Nat, A] { 
    val dim : N
    def leibniz : T === Tree[N, A]
  }

  trait IsConsTree[T, N <: Nat, G[_], A] {
    val dim : N
    def leibniz : T === Tree[N, G[A]]
  }

  trait IsDerivative[T, N <: Nat, A] {
    val dim : N
    def leibniz : T === Derivative[N, A]
  }

  trait IsContext[T, N <: Nat, A] {
    val dim : N
    def leibniz : T === Context[N, A]
  }

  implicit def treesAreTrees[N <: Nat, A](implicit n : N) : IsTree[Tree[N, A], N, A] = 
    new IsTree[Tree[N, A], N, A] {
      val dim = n
      def leibniz : Tree[N, A] === Tree[N, A] = 
        refl[Tree[N, A]]
    }

  implicit def consTreesAreConsTrees[N <: Nat, G[_], A](implicit n : N) : IsConsTree[Tree[N, G[A]], N, G, A] =
    new IsConsTree[Tree[N, G[A]], N, G, A] {
      val dim = n
      def leibniz : Tree[N, G[A]] === Tree[N, G[A]] =
        refl[Tree[N, G[A]]]
    }

  implicit def contextsAreContexts[N <: Nat, A](implicit n : N) : IsContext[Context[N, A], N, A] =
    new IsContext[Context[N, A], N, A] {
      val dim = n
      def leibniz : Context[N, A] === Context[N, A] =
        refl[Context[N, A]]
    }

  implicit def derivsAreDerivs[N <: Nat, A](implicit n : N) : IsDerivative[Derivative[N, A], N, A] =
    new IsDerivative[Derivative[N, A], N, A] {
      val dim = n
      def leibniz : Derivative[N, A] === Derivative[N, A] =
        refl[Derivative[N, A]]
    }

  implicitly[IsTree[Tree[_0, Int], _0, Int]]
  implicitly[IsTree[Tree[_1, Int], _1, Int]]
  implicitly[IsTree[Tree[_2, Int], _2, Int]]
  implicitly[IsTree[Tree[_3, Int], _3, Int]]

  implicitly[IsConsTree[Tree[_3, List[Int]], _3, List, Int]]

  implicitly[IsDerivative[Derivative[_3, Int], _3, Int]]

  //============================================================================================
  // OPERATIONS CLASSES
  //

  implicit class TreeOps[T, N <: Nat, A](t : T)(implicit val isTree : IsTree[T, N, A]) {

    import isTree._

    val tree : Tree[N, A] = 
      subst(t)(leibniz)

    def map[B](f : A => B) : Tree[N, B] = 
      TreeLib.mapT[N, A, B](dim, tree, f)

    // The signature here doesn't really fit.  Could use an unapply to witness that G has been
    // applied to A .....
    // def sequence[G[_], B](f : A => G[B])(implicit apG : Applicative[G]) : G[Tree[N, A]] =
    //   ???

    def zipComplete[B](other : Tree[N, B]) : Option[Tree[N, (A, B)]] = 
      TreeLib.zipComplete[N, A, B](dim, tree, other)

  }

  implicit class DerivativeOps[D, N <: Nat, A](d : D)(implicit val isDerivative : IsDerivative[D, N, A]) {

    import isDerivative._

    val deriv : Derivative[N, A] =
      subst(d)(leibniz)

    def plugWith(a : A) : Tree[N, A] = ???

  }

  //============================================================================================
  // DIMENSION MATCHING
  //

  trait ZeroDimMatch[N <: Nat] extends ZeroMatch[N]
  trait OneDimMatch[N <: Nat] extends OneMatch[N]

  trait SuccDimMatch[N <: Nat] extends SuccMatch[N] {

    type PF[+A] = Tree[P, A]

  }

  trait DblSuccDimMatch[N <: Nat] extends DblSuccMatch[N] {

    type PPF[+A] = Tree[PP, A]
    type PF[+A] = Tree[P, A]

  }

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

    // def isTraverse[N <: TreeIndex](n : N) : Traverse[N#Tree] =
    //   new Traverse[N#Tree] {
    //     def traverseImpl[G[_], A, B](tr : Tree[N, A])(f : A => G[B])(implicit apG : Applicative[G]) : G[Tree[N, B]] =
    //       traverseT(n, tr, f)
    //   }

      def mapT[N <: Nat, A, B](n : N, tree : Tree[N, A], f : A => B) : Tree[N, B] = ???
    //     isTraverse(n).map(tree)(f)

    //   def traverseT[N <: TreeIndex, G[_], A, B](n : N, tree :  Tree[N, A], f : A => G[B])(implicit apG : Applicative[G]) : G[Tree[N, B]] =
    //     n match {
    //       case IsZeroIndex(zm) => { import zm._ ;
    //         rewrite[G, Tree[_0, B], Tree[N, B]](f(tree : Tree[_0, A]))
    //       }
    //       case IsSuccIndex(sm) => { import sm._ ;
    //         implicit val PT : Traverse[P#Tree] = isTraverse(p)
    //         val ST = implicitly[Traverse[ST[P]#Tree]]

    //         rewrite[G, Tree[ST[P], B], Tree[N, B]](
    //           ST.traverse(tree : Tree[ST[P], A])(f)
    //         )
    //       }
    //     }

    def sequenceT[N <: Nat, G[_], A](n : N, tree : Tree[N, G[A]])(implicit apG : Applicative[G]) : G[Tree[N, A]] = ???
    //     traverseT(n, tree, identity[G[A]])

    def const[N <: Nat, A, B](n : N, b : B, tree : Tree[N, A]) : Tree[N, B] =
      mapT[N, A, B](n, tree, (_ => b))

    def shapeOf[N <: Nat, A](n : N, tree : Tree[N, A]) : Tree[N, Unit] =
      const(n, (), tree)

    // Be careful about the possible infinite loop arising from using to many implicit classes
    // in this code ....
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

              val dd : Derivative[P, Tree[S[P], A]] = d

              // close[S[P], A](S(p), cs, joint(a , plug[P, Tree[S[P], A]](p, d, t)))

              close[S[P], A](S(p), cs, joint(a , dd plugWith t))
            }
          }
        }
      }


  def visit[N <: Nat, A](n : N, d : Direction[N], z : Zipper[N, A]) : Option[Zipper[N, A]] = 
    n match {
      case IsZeroDim(zm) => None
      case IsOneDim(om) => { import om._ ;

        val ud : Direction[_1] = d
        val uz : Zipper[_1, A] = z

        (ud , uz) match {
          case (Nil, (focus , cntxt)) => {
            (focus : Slice[Id, A]) match {
              case Cap() => None
              case Joint(head, tail) => {
                Some((tail, (head, ()) :: cntxt) : Zipper[_1, A])
              }
            }
          }
          case (_, _) => None
        }

      }
      case IsDblSuccDim(dm) => { import dm._ ;

        val ud : Direction[S[P]] = d
        val uz : Zipper[S[P], A] = z

        (ud , uz) match {
          case (addr, (focus , cntxt)) => {
            (focus : Slice[PF, A]) match {
              case Cap() => None
              case Joint(a, shell) => {

                def visitBranch(zp : Zipper[P, Tree[S[P], A]]) : Option[Zipper[N, A]] = {

                  val f : Slice[PPF, Tree[S[P], A]] = zp._1

                  f match {
                    case Cap() => None
                    case Joint(t, tsh) => {
                      Some((t, (a, (tsh, zp._2)) :: cntxt) : Zipper[S[P], A])
                    }

                  }
                }

                for {
                  shellContext <- seek[P, Tree[S[P], A]](p, addr, (shell, Nil))
                  zipAtBranch <- visitBranch(shellContext)
                } yield zipAtBranch

              }
            }
          }
        }
      }
    }

    def seek[N <: Nat, A](n : N, a : Address[N], z : Zipper[N, A]) : Option[Zipper[N, A]] =
      a match {
        case Nil => Some(z)
        case d :: ds =>
          for {
            zz <- seek(n, ds, z)
            zv <- visit(n, d, zz)
          } yield zv
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
                zippedShell <- sequenceT(p,
                  (mapT[P, (Tree[S[P], A], Tree[S[P], B]), Option[Tree[S[P], (A, B)]]](p, branchPairs, {
                    case (t1 : Tree[S[P], A], t2 : Tree[S[P], B]) => zipComplete[S[P], A, B](S(p), t1, t2)
                  }))
                )
              } yield joint((a, b), zippedShell)
            }
            case (_, _) => None
          }
        }
      }


  }

}

// trait TreeFunctions {



//   def nGlob[N <: TreeIndex, A](n : N, a : A) : N#Tree[A] =
//     n match {
//       case IsZeroIndex(zm) => { import zm._ ; a }
//       case IsSuccIndex(sm) => { import sm._ ; 
//         Joint(a, nGlob[P, STree[A]](p, Cap()))
//       }
//     }

//   def globDeriv[N <: TreeIndex](n : N) : N#Derivative[ST[N]#Address] =
//     n match {
//       case IsZeroIndex(zm) => { import zm._ ; () }
//       case IsSuccIndex(sm) => { import sm._ ;  
//         (nGlob(p, (Cap() : STree[ST[N]#Address])), Nil)
//       }
//     }

//   def zipComplete[N <: TreeIndex, A, B](n : N, ta : N#Tree[A], tb : N#Tree[B]) : Option[N#Tree[(A, B)]] =
//     n match {
//       case IsZeroIndex(zm) => { import zm._ ; 

//         val a : ZT.type#Tree[A] = ta
//         val b : ZT.type#Tree[B] = tb

//         Some((a, b))

//       }
//       case IsSuccIndex(sm) => { import sm._ ;

//         val sta : STree[A] = ta
//         val stb : STree[B] = tb

//         (sta, stb) match {
//           case (Cap(), Cap()) => Some(Cap())
//           case (Joint(a, ash), Joint(b, bsh)) => {

//             for {
//               branchPairs <- zipComplete[P, STree[A], STree[B]](p, ash, bsh)
//               zippedShell <- sequenceT(p,
//                 (mapT[P, (STree[A], STree[B]), Option[STree[(A, B)]]](p, branchPairs, {
//                   case (t1 : STree[A], t2 : STree[B]) => zipComplete[ST[P], A, B](ST(p), t1, t2)
//                 }))
//               )
//             } yield Joint((a, b), zippedShell)

//           }
//           case (_, _) => None
//         }
//       }     
//     }

//   def zipWithCorolla[N <: TreeIndex, A](n : N, tree : N#Tree[A]) : N#Tree[(A, N#Derivative[A])] =
//     n match {
//       case IsZeroIndex(zm) => { import zm._ ; 
//         val a : A = (tree : ZT.type#Tree[A])
//         (a, () : N#Derivative[A])
//       }
//       case IsSuccIndex(sm) => { import sm._ ;
//         (tree : STree[A]) match {
//           case Cap() => Cap()
//           case Joint(a, shell) => {

//             val shellCorollas : PTree[STree[(A, N#Derivative[A])]] = 
//               mapT[P, STree[A], STree[(A, N#Derivative[A])]](p, shell, 
//                 (t : STree[A]) => {
//                   rewrite[({ type L[+X] = STree[(A, X)] })#L, ST[P]#Derivative[A], N#Derivative[A]](
//                     zipWithCorolla[ST[P], A](ST(p), t)
//                   )
//                 })

//             val thisCorolla : N#Derivative[A] = 
//               (const(p, Cap(), shell) : PTree[STree[A]] , Nil)

//             Joint((a, thisCorolla), shellCorollas)

//           }
//         }
//       }
//     }

//   def zipWithPrefix[N <: TreeIndex, A](n : N, pref : N#Address, tree : N#Tree[A]) : N#Tree[(A, N#Address)] =
//     n match {
//       case IsZeroIndex(zm) => { import zm._ ;
//         val a : A = (tree : ZT.type#Tree[A])
//         (a , Nil)
//       }
//       case IsSuccIndex(sm) => { import sm._ ;
//         (tree : STree[A]) match {
//           case Cap() => Cap()
//           case Joint(a, shell) => {

//             val shellWithAddrs : PTree[(STree[A], P#Address)] =
//               zipWithAddress(p, shell)

//             val shellResult : PTree[STree[(A, N#Address)]] =
//               mapT[P, (STree[A], P#Address), STree[(A, N#Address)]](p, shellWithAddrs, {
//                 case (st, ad) => {
//                   rewrite[({ type L[X] = STree[(A, X)] })#L, ST[P]#Address, N#Address](
//                     zipWithPrefix[ST[P], A](ST(p), ad :: (pref : ST[P]#Address), st)
//                   )
//                 }
//               })
              
//             Joint((a, pref), shellResult)
//           }
//         }
//       }
//     }

//   def zipWithAddress[N <: TreeIndex, A](n : N, tree : N#Tree[A]) : N#Tree[(A, N#Address)] =
//     zipWithPrefix(n, Nil, tree)

//   def flattenWithPrefix[N <: TreeIndex, A](
//     n : N, 
//     prefix : ST[N]#Address, 
//     corolla : N#Derivative[ST[N]#Address], 
//     tree : ST[N]#Tree[A]
//   ) : Option[N#Tree[ST[N]#Address]] = 
//     n match {
//       case IsZeroIndex(zm) => { import zm._ ; 
//         tree match {
//           case Cap() => Some(plug(n, corolla, prefix))
//           case Joint(head, tail) => {
//             val ocs = zm.succConversions
//             import ocs.{treeCohEq => succTreeCohEq,
//                         treeCoeEq => succTreeCoeEq,
//                         addressCohEq => succAddressCohEq,
//                         addressCoeEq => succAddresCoeEq}

//             implicit val resultEq : _0#Tree[_1#Address] === N#Tree[ST[N]#Address] = 
//               lift[Nothing, Nothing, Any, Any, _0#Tree, _1#Address, ST[N]#Address](
//                 implicitly[_1#Address === ST[N]#Address]
//               ).andThen(
//                 implicitly[_0#Tree[ST[N]#Address] === N#Tree[ST[N]#Address]]
//               )

//             val tailRewrite : _0#Tree[_1#Tree[A]] = 
//               rewrite[_0#Tree, ST[N]#Tree[A], _1#Tree[A]](tail)

//             resultEq.subst[Option](
//               flattenWithPrefix[_0, A](ZT, Nil :: prefix, (), tailRewrite)
//             )
//           }
//         }
//       }
//       case IsSuccIndex(sm) => { import sm._ ; 
//         tree match {
//           case Cap() => Some(plug(n, corolla, prefix))
//           case Joint(a, shell) => {
//             if ((shell : STree[ST[N]#Tree[A]]).isCap) Some(Cap()) else {
//               val ssm = sm.succConversions

//               import ssm.{
//                 treeCohEq => succTreeCohEq,
//                 treeCoeEq => succTreeCoeEq,
//                 addressCohEq => succAddressCohEq,
//                 addressCoeEq => succAddresCoeEq
//               }

//               val graftShell : STree[(ST[ST[P]]#Address, ST[P]#Derivative[ST[ST[P]]#Address])] = 
//                 mapT[ST[P], 
//                   ((ST[ST[P]]#Address, ST[P]#Derivative[ST[ST[P]]#Address]) , ST[P]#Address), 
//                   (ST[ST[P]]#Address, ST[P]#Derivative[ST[ST[P]]#Address])
//                 ](ST(p), 
//                   zipWithAddress[ST[P], (ST[ST[P]]#Address, ST[P]#Derivative[ST[ST[P]]#Address])](ST(p),
//                     zipWithCorolla[ST[P], ST[ST[P]]#Address](ST(p),
//                       rewrite[STree, ST[N]#Address, ST[ST[P]]#Address](
//                         const(n, prefix, shell)
//                       )
//                     )
//                   ),
//                   { case ((at, d) , ah) => (ah :: at, d) }
//                 )

//               for {
//                 zippedShells <- (
//                   zipComplete[ST[P],
//                     (ST[ST[P]]#Address, ST[P]#Derivative[ST[ST[P]]#Address]),
//                     ST[ST[P]]#Tree[A]
//                   ](ST(p), graftShell,
//                     rewrite[STree, ST[N]#Tree[A], ST[ST[P]]#Tree[A]](shell)
//                   )
//                 )

//                 flattenedShell <- (
//                   sequenceT[ST[P], Option, STree[ST[ST[P]]#Address]](ST(p), 
//                     mapT[ST[P],
//                       ((ST[ST[P]]#Address, ST[P]#Derivative[ST[ST[P]]#Address]), ST[ST[P]]#Tree[A]),
//                       Option[STree[ST[ST[P]]#Address]]
//                     ](ST(p), zippedShells,
//                       { case ((pr , cn) , tr) => flattenWithPrefix[ST[P], A](ST(p), pr, cn, tr) }
//                     )
//                   )
//                 )

//                 result <- substitute[ST[P], ST[ST[P]]#Address](ST(p), flattenedShell)

//               } yield {
//                 rewrite[STree, ST[ST[P]]#Address, ST[N]#Address](result)
//               }
//             }
//           }
//         }
//       }
//     }

//   def flattenWithAddress[N <: TreeIndex, A](n : N, tree : ST[N]#Tree[A]) : Option[N#Tree[ST[N]#Address]] =
//     flattenWithPrefix[N, A](n, Nil, globDeriv(n), tree)

//   def flatten[N <: TreeIndex, A](n : N, tree : ST[N]#Tree[A]) : Option[N#Tree[Unit]] =
//     for {
//       addrTree <- flattenWithAddress[N, A](n, tree)
//     } yield shapeOf(n, addrTree)

//   def graft[N <: TreeIndex, A](n : N, tree : ST[N]#Tree[A], brs : N#Tree[ST[N]#Tree[A]]) : Option[ST[N]#Tree[A]] = {

//     type Grafter[X] = StateT[Option, ST[N]#Tree[A], X]
//     type GrafterS[S, X] = StateT[Option, S, X]
//     type GrafterT[M[+_], X] = StateT[M, ST[N]#Tree[A], X]

//     val GS = MonadState[GrafterS, ST[N]#Tree[A]]
//     val GT = MonadTrans[GrafterT]

//     import GS._
//     import GT._

//     for {
//       addrTr <- flattenWithAddress[N, A](n, tree)
//       grafts <- zipComplete[N, ST[N]#Address, ST[N]#Tree[A]](n, addrTr, brs)

//       result <- sequenceT[N, Grafter, Unit](n,
//         mapT[N,
//           (ST[N]#Address, ST[N]#Tree[A]),
//           Grafter[Unit]
//         ](n, grafts, {
//           case (addr, br) => {
//             for {
//               curTr <- get
//               graftResult <- liftM[Option, ST[N]#Tree[A]](
//                 graftAt(n, addr, curTr, br)
//               )
//               _ <- put(graftResult)
//             } yield ()
//           }
//         })
//       ).exec(tree)

//     } yield result
//   }

//   def graftAt[N <: TreeIndex, A](n : N, addr : ST[N]#Address, tree : ST[N]#Tree[A], br : ST[N]#Tree[A]) : Option[ST[N]#Tree[A]] =
//     seek[ST[N], A](ST(n), addr, (tree, Nil)) match {
//       case None => None
//       case Some((Cap(), c)) => Some(close[ST[N], A](ST(n), c, br))
//       case Some((Joint(_, _), _)) => None
//     }

//   def substitute[N <: TreeIndex, A](n : N, ttree : N#Tree[N#Tree[A]]) : Option[N#Tree[A]] =
//     n match {
//       case IsZeroIndex(zm) => { import zm._ ; 
//         val strip1 : ZT.type#Tree[N#Tree[A]] = ttree
//         val strip2 : ZT.type#Tree[A] = strip1
//         Some(strip2)
//       }
//       case IsSuccIndex(sm) => { import sm._ ; 
//         (ttree : STree[N#Tree[A]]) match {
//           case Cap() => Some(Cap())
//           case Joint(t, sts) => {
//             for {
//               s <- sequenceT(p,
//                 mapT[P, STree[N#Tree[A]], Option[STree[A]]](p, sts,
//                   (tt : STree[N#Tree[A]]) => {
//                     rewrite[Option, N#Tree[A], STree[A]](
//                       substitute[N, A](n, tt)
//                     )
//                   }
//                 )
//               )

//               g <- graft[P, A](p, t, s)

//             } yield g
//           }
//         }
//       }
//     }

// }

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
