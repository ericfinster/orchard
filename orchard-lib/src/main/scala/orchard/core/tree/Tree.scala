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

    def constantWith[B](b : B) : Tree[N, B] = 
      TreeLib.const(dim, b, tree)

    def zipComplete[B](other : Tree[N, B]) : Option[Tree[N, (A, B)]] = 
      TreeLib.zipComplete[N, A, B](dim, tree, other)

    def zipWithCorolla : Tree[N, (A, Derivative[N, A])] = 
      TreeLib.zipWithCorolla[N, A](dim, tree)

    def zipWithAddress : Tree[N, (A, Address[N])] =
      TreeLib.zipWithAddress[N, A](dim, tree)

  }

  implicit class DerivativeOps[D, N <: Nat, A](d : D)(implicit val isDerivative : IsDerivative[D, N, A]) {

    import isDerivative._

    val deriv : Derivative[N, A] =
      subst(d)(leibniz)

    def plugWith(a : A) : Tree[N, A] = 
      TreeLib.plug(dim, deriv, a)

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
  implicit def liftSucc[M <: Nat, N <: Nat, A](implicit eq : M === N) : S[M] === S[N] = 
    force[Nothing, Any, S[M], S[N]]

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

  implicit def liftAddr[M <: Nat, N <: Nat](implicit eq : M === N) : Address[M] === Address[N] = 
    lift[Nothing, Nothing, Any, Any, List, Direction[M], Direction[N]](liftDir[M, N])

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

  implicit def addrCoerce[M <: Nat, N <: Nat](a : Address[M])(implicit eq : M === N) : Address[N] = 
    subst(a)(implicitly[Address[M] === Address[N]])

  //============================================================================================
  // TREE FUNCTIONS
  //

  object TreeLib {

    def isTraverse[N <: Nat](n : N) : Traverse[({ type L[+X] = Tree[N, X] })#L] =
      new Traverse[({ type L[+X] = Tree[N, X] })#L] {
        def traverseImpl[G[_], A, B](tr : Tree[N, A])(f : A => G[B])(implicit apG : Applicative[G]) : G[Tree[N, B]] =
          traverseT(n, tr, f)
      }

    def mapT[N <: Nat, A, B](n : N, tree : Tree[N, A], f : A => B) : Tree[N, B] = 
      isTraverse(n).map(tree)(f)

    def traverseT[N <: Nat, G[_], A, B](n : N, tree :  Tree[N, A], f : A => G[B])(implicit apG : Applicative[G]) : G[Tree[N, B]] =
      n match {
        case IsZeroDim(zm) => { import zm._ ;
          rewrite[G, Tree[_0, B], Tree[N, B]](f(tree : Tree[_0, A]))
        }
        case IsSuccDim(sm) => { import sm._ ;

          type NSlice[+X] = Slice[PF, X]

          implicit val PT : Traverse[PF] = isTraverse(p)
          val ST = implicitly[Traverse[NSlice]]

          val tt : Tree[S[P], A] = tree

          rewrite[G, Tree[S[P], B], Tree[N, B]](
            ST.traverse(tt : Slice[PF, A])(f)
          )
        }
      }

    def sequenceT[N <: Nat, G[_], A](n : N, tree : Tree[N, G[A]])(implicit apG : Applicative[G]) : G[Tree[N, A]] =
      traverseT(n, tree, identity[G[A]])

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


    def nGlob[N <: Nat, A](n : N, a : A) : Tree[N, A] =
      n match {
        case IsZeroDim(zm) => { import zm._ ; a : Tree[_0, A] }
        case IsSuccDim(sm) => { import sm._ ; 
          joint(a, nGlob[P, Tree[S[P], A]](p, Cap[PF, A]()))
        }
      }

    def globDeriv[N <: Nat](n : N) : Derivative[N, Address[S[N]]] = 
      n match {
        case IsZeroDim(zm) => { import zm._ ; () : Derivative[_0, Address[S[N]]]}
        case IsSuccDim(sm) => { import sm._ ;  
          (nGlob(p, (Cap[PF, Address[S[N]]]())), Nil) : Derivative[S[P], Address[S[N]]]
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
                zippedShell <- sequenceT(p,
                  branchPairs map {
                    case (t1 : Tree[S[P], A], t2 : Tree[S[P], B]) => zipComplete[S[P], A, B](S(p), t1, t2)
                  }
                )
              } yield joint((a, b), zippedShell)

            }
            case (_, _) => None
          }
        }
      }

    def zipWithCorolla[N <: Nat, A](n : N, tree : Tree[N, A]) : Tree[N, (A, Derivative[N, A])] =
      n match {
        case IsZeroDim(zm) => { import zm._ ;
          val a : A = (tree : Tree[_0, A])
          val d : Derivative[_0, A] = ()
          val t : Tree[_0, (A, Derivative[N, A])] = (a, d)
          t
        }
        case IsSuccDim(sm) => { import sm._ ; 

          val tt : Tree[S[P], A] = tree

          (tt : Slice[PF, A]) match {
            case Cap() => {
              Cap[PF, (A, Derivative[N, A])]() : Tree[S[P], (A, Derivative[N, A])]
            }
            case Joint(a, shell) => {

              val shellCorollas : Tree[P, Tree[S[P], (A, Derivative[N, A])]] =
                shell map ((t : Tree[S[P], A]) => {
                  rewrite[({ type L[+X] = Tree[S[P], (A, X)] })#L, Derivative[S[P], A], Derivative[N, A]](
                    zipWithCorolla[S[P], A](S(p), t)
                  )
                })

              val thisCorolla : Derivative[S[P], A] =
                (const(p, cap, shell) : Tree[P, Tree[S[P], A]] , Nil)

              joint((a, thisCorolla), shellCorollas) : Tree[S[P], (A, Derivative[N, A])]

            }
          }
        }
      }

    def zipWithPrefix[N <: Nat, A](n : N, pref : Address[N], tree : Tree[N, A]) : Tree[N, (A, Address[N])] =
      n match {
        case IsZeroDim(zm) => { import zm._ ;
          val a : A = (tree : Tree[_0, A])
          val t : Tree[_0, (A, Address[N])] = (a, Nil)
          t
        }
        case IsSuccDim(sm) => { import sm._ ;

          val tt : Tree[S[P], A] = tree

          (tt : Slice[PF, A]) match {
            case Cap() => {
              Cap[PF, (A, Address[N])]() : Tree[S[P], (A, Address[N])]
            }
            case Joint(a, shell) => {

              val shellWithAddrs : Tree[P, (Tree[S[P], A], Address[P])] =
                zipWithAddress(p, shell)

              val shellResult : Tree[P, Tree[S[P], (A, Address[N])]] = 
                shellWithAddrs map {
                  case (st, ad) => {
                    rewrite[({ type L[X] = Tree[S[P], (A, X)] })#L, Address[S[P]], Address[N]](
                      zipWithPrefix[S[P], A](S(p), ad :: (pref : Address[S[P]]), st)
                    )
                  }
                }

              joint((a, pref), shellResult) : Tree[S[P], (A, Address[N])]
            }
          }
        }
      }

    def zipWithAddress[N <: Nat, A](n : N, tree : Tree[N, A]) : Tree[N, (A, Address[N])] =
      zipWithPrefix(n, Nil, tree)

    def flattenWithPrefix[N <: Nat, A](
      n : N,
      prefix : Address[S[N]],
      corolla : Derivative[N, Address[S[N]]],
      tree : Tree[S[N], A]
    ) : Option[Tree[N, Address[S[N]]]] = 
      n match {
        case IsZeroDim(zm) => { import zm._ ; 

          val tt : Tree[_1, A] = tree

          (tt : Slice[Id, A]) match {
            case Cap() => Some(plug(n, corolla, prefix))
            case Joint(head, tail) => {

              implicit val resultEq : Tree[_0, Address[_1]] === Tree[N, Address[S[N]]] = 
                lift[Nothing, Nothing, Any, Any, Tree0, Address[_1], Address[S[N]]](
                  implicitly[Address[_1] === Address[S[N]]]
                ).andThen(
                  implicitly[Tree[_0, Address[S[N]]] === Tree[N, Address[S[N]]]]
                )

              resultEq.subst[Option](
                flattenWithPrefix[_0, A](Z, Nil :: prefix, (), tail)
              )
            }
          }

        }
        case IsSuccDim(sm) => { import sm._ ; 

          val tt : Tree[S[S[P]], A] = tree

          type NTree[+A] = Slice[PF, A]

          (tt : Slice[NTree, A]) match {
            case Cap() => Some(plug(n, corolla, prefix))
            case Joint(a, shell) => {
              if (shell.isCap) {
                Some(
                  Cap[PF, Address[S[N]]]() : Tree[S[P], Address[S[N]]]
                )
              } else {

                val spShell : Tree[S[P], Tree[S[S[P]], A]] = shell
                val sspAddr : Address[S[S[P]]] = prefix

                val graftShell : Tree[S[P], (Address[S[S[P]]], Derivative[S[P], Address[S[S[P]]]])]= 
                  (spShell constantWith sspAddr).zipWithCorolla.zipWithAddress map {
                    case ((at, d), ah) => (ah :: at, d)
                  }

                for {
                  zippedShells <- (
                    graftShell zipComplete spShell
                  )

                  flattenedShell <- (
                    sequenceT[S[P], Option, Tree[S[P], Address[S[S[P]]]]](S(p),
                      zippedShells map {
                        case ((pr, cn), tr) => flattenWithPrefix[S[P], A](S(p), pr, cn, tr)
                      }
                    )
                  )

                  result <- substitute[S[P], Address[S[S[P]]]](S(p), flattenedShell)

                } yield {
                  val resultRewrite : NTree[Address[S[N]]] = 
                    rewrite[NTree, Address[S[S[P]]], Address[S[N]]](result)

                  resultRewrite : Tree[S[P], Address[S[N]]]
              }
              }
            }
          }
        }
      }

    def flattenWithAddress[N <: Nat, A](n : N, tree : Tree[S[N], A]) : Option[Tree[N, Address[S[N]]]] =
      flattenWithPrefix[N, A](n, Nil, globDeriv(n), tree)

    def flatten[N <: Nat, A](n : N, tree : Tree[S[N], A]) : Option[Tree[N, Unit]] =
      for {
        addrTree <- flattenWithAddress[N, A](n, tree)
      } yield shapeOf(n, addrTree)


    def graft[N <: Nat, A](n : N, tree : Tree[S[N], A], brs : Tree[N, Tree[S[N], A]]) : Option[Tree[S[N], A]] = {

      type Grafter[X] = StateT[Option, Tree[S[N], A], X]
      type GrafterS[S, X] = StateT[Option, S, X]
      type GrafterT[M[+_], X] = StateT[M, Tree[S[N], A], X]

      val GS : MonadState[GrafterS, Tree[S[N], A]] = MonadState[GrafterS, Tree[S[N], A]]
      val GT : MonadTrans[GrafterT] = MonadTrans[GrafterT]

      import GS._
      import GT._

      implicit val nn : N = n

      for {
        addrTr <- flattenWithAddress[N, A](n, tree)
        grafts <- zipComplete[N, Address[S[N]], Tree[S[N], A]](n, addrTr, brs)

        result <- sequenceT[N, Grafter, Unit](n,
          grafts map {
            case (addr, br) => {
              for {
                curTr <- get
                graftResult <- liftM[Option, Tree[S[N], A]](
                  graftAt(n, addr, curTr, br)
                )
                _ <- put(graftResult)
              } yield ()
            }
          }
        ).exec(tree)

      } yield result
    }

    def graftAt[N <: Nat, A](n : N, addr : Address[S[N]], tree : Tree[S[N], A], br : Tree[S[N], A]) : Option[Tree[S[N], A]] = {
      seek[S[N], A](S(n), addr, (tree, Nil)) match {
        case None => None
        case Some(z) => {

          type NT[+X] = Tree[N, X]

          // I think at least this kind of case could be dealt with by a custom extractor ...
          val f : Slice[NT, A] = z._1

          f match {
            case Cap() => {
              Some(close[S[N], A](S(n), z._2, br))
            }
            case Joint(_, _) => None
          }
        }
      }
    }

    def substitute[N <: Nat, A](n : N, ttree : Tree[N, Tree[N, A]]) : Option[Tree[N, A]] = 
      n match {
        case IsZeroDim(zm) => { import zm._ ;

          implicit val nn = n

          Some(
            ttree map ((tree : Tree[N, A]) => {
              val tt : Tree[_0, A] = tree
              tt
            }) : Tree[N, A]
          )

        }
        case IsSuccDim(sm) => { import sm._ ; 

          val stree : Tree[S[P], Tree[N, A]] = ttree

          (stree : Slice[PF, Tree[N, A]])  match {
            case Cap() => Some(Cap[PF, A]() : Tree[S[P], A])
            case Joint(t, sts) =>
              for {
                s <- sequenceT(p,
                  sts map ((tt : Tree[S[P], Tree[N, A]]) => {
                    rewrite[Option, Tree[N, A], Tree[S[P], A]](
                      substitute[N, A](n, tt)
                    )
                  })
                )

                g <- graft[P, A](p, t, s)

              } yield g

          }
        }
      }

  }

  //============================================================================================
  // TESTING
  //

}
