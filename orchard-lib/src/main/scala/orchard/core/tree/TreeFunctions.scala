/**
  * TreeFunctions.scala - Implementation of Agda code for hdts
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

trait TreeFunctions {

  import TreeIndex._

  def isTraverse[N <: TreeIndex](n : N) : Traverse[N#Tree] = 
    new Traverse[N#Tree] {
      def traverseImpl[G[_], A, B](tr : Tree[N, A])(f : A => G[B])(implicit apG : Applicative[G]) : G[Tree[N, B]] = 
        traverseT(n, tr, f)
    }

  def mapT[N <: TreeIndex, A, B](n : N, tree : Tree[N, A], f : A => B) : Tree[N, B] = 
    isTraverse(n).map(tree)(f)

  def traverseT[N <: TreeIndex, G[_], A, B](n : N, tree :  Tree[N, A], f : A => G[B])(implicit apG : Applicative[G]) : G[Tree[N, B]] = 
    n match {
      case IsZeroIndex(zm) => { import zm._ ;
        rewrite[G, Tree[_0, B], Tree[N, B]](f(tree : Tree[_0, A]))
      }
      case IsSuccIndex(sm) => { import sm._ ; 
        implicit val PT : Traverse[P#Tree] = isTraverse(p)
        val ST = implicitly[Traverse[ST[P]#Tree]]

        rewrite[G, Tree[ST[P], B], Tree[N, B]](
          ST.traverse(tree : Tree[ST[P], A])(f)
        )
      }
    }

  def sequenceT[N <: TreeIndex, G[_], A](n : N, tree : Tree[N, G[A]])(implicit apG : Applicative[G]) : G[Tree[N, A]] =
    traverseT(n, tree, identity[G[A]])

  def const[N <: TreeIndex, A, B](n : N, b : B, tree : Tree[N, A]) : Tree[N, B] =
    mapT[N, A, B](n, tree, (_ => b))

  def shapeOf[N <: TreeIndex, A](n : N, tree : Tree[N, A]) : Tree[N, Unit] =
    const(n, (), tree)

  def plug[N <: TreeIndex, A](n : N, d : Derivative[N, A], a : A) : Tree[N, A] =
    n match {
      case IsZeroIndex(zm) => { import zm._ ; (a : Tree[_0, A]) }
      case IsSuccIndex(sm) => { import sm._ ;
        (d : Derivative[ST[P], A]) match {
          case (shell, context) => {
            close[ST[P], A](ST(p), context, Joint(a, shell))
          }
        }
      }
    }

  def close[N <: TreeIndex, A](n : N, c : Context[N, A], t : Tree[N, A]) : Tree[N, A] =
    n match {
      case IsZeroIndex(zm) => { import zm._ ; t }
      case IsSuccIndex(sm) => { import sm._ ;
        (c : Context[ST[P], A]) match {
          case Nil => t
          case (a , d) :: cs => {
            close[ST[P], A](ST(p), cs, Joint(a , plug[P, Tree[ST[P], A]](p, d, t)))
          }
        }
      }
    }

  def visit[N <: TreeIndex, A](n : N, d : Direction[N], z : Zipper[N, A]) : Option[Zipper[N, A]] =
    n match {
      case IsZeroIndex(zm) => None
      case IsOneIndex(om) => { import om._ ;

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
      case IsDblSuccIndex(dcs) => { import dcs._ ;

        val ud : Direction[ST[P]] = d
        val uz : Zipper[ST[P], A] = z

        (ud , uz) match {
          case (addr, (focus , cntxt)) => {
            (focus : Slice[FS, A]) match {
              case Cap() => None
              case Joint(a, shell) => {

  //               def visitBranch(zp : P#Zipper[ST[P]#Tree[A]]) : Option[N#Zipper[A]] =
  //                 zp match {
  //                   case (Cap(), _) => None
  //                   case (Joint(t, tsh), z0) => {
  //                     Some((t , (a , (tsh, z0)) :: cntxt) : N#Zipper[A])
  //                   }
  //                 }

  //               for {
  //                 shellContext <- seek[P, ST[P]#Tree[A]](p, addr, (shell, Nil))
  //                 zipAtBranch <- visitBranch(shellContext)
  //               } yield zipAtBranch

                ???
              }
            }
          }
        }
      }
    }

  // def seek[N <: TreeIndex, A](n : N, a : N#Address, z : N#Zipper[A]) : Option[N#Zipper[A]] = 
  //   a match {
  //     case Nil => Some(z)
  //     case d :: ds => 
  //       for {
  //         zz <- seek(n, ds, z)
  //         zv <- visit(n, d, zz)
  //       } yield zv
  //   }

  // def nGlob[N <: TreeIndex, A](n : N, a : A) : N#Tree[A] =
  //   n match {
  //     case IsZeroIndex(zm) => { import zm._ ; a }
  //     case IsSuccIndex(sm) => { import sm._ ; 
  //       Joint(a, nGlob[P, STree[A]](p, Cap()))
  //     }
  //   }

  // def globDeriv[N <: TreeIndex](n : N) : N#Derivative[ST[N]#Address] =
  //   n match {
  //     case IsZeroIndex(zm) => { import zm._ ; () }
  //     case IsSuccIndex(sm) => { import sm._ ;  
  //       (nGlob(p, (Cap() : STree[ST[N]#Address])), Nil)
  //     }
  //   }

  // def zipComplete[N <: TreeIndex, A, B](n : N, ta : N#Tree[A], tb : N#Tree[B]) : Option[N#Tree[(A, B)]] =
  //   n match {
  //     case IsZeroIndex(zm) => { import zm._ ; 

  //       val a : ZT.type#Tree[A] = ta
  //       val b : ZT.type#Tree[B] = tb

  //       Some((a, b))

  //     }
  //     case IsSuccIndex(sm) => { import sm._ ;

  //       val sta : STree[A] = ta
  //       val stb : STree[B] = tb

  //       (sta, stb) match {
  //         case (Cap(), Cap()) => Some(Cap())
  //         case (Joint(a, ash), Joint(b, bsh)) => {

  //           for {
  //             branchPairs <- zipComplete[P, STree[A], STree[B]](p, ash, bsh)
  //             zippedShell <- sequenceT(p,
  //               (mapT[P, (STree[A], STree[B]), Option[STree[(A, B)]]](p, branchPairs, {
  //                 case (t1 : STree[A], t2 : STree[B]) => zipComplete[ST[P], A, B](ST(p), t1, t2)
  //               }))
  //             )
  //           } yield Joint((a, b), zippedShell)

  //         }
  //         case (_, _) => None
  //       }
  //     }     
  //   }

  // def zipWithCorolla[N <: TreeIndex, A](n : N, tree : N#Tree[A]) : N#Tree[(A, N#Derivative[A])] =
  //   n match {
  //     case IsZeroIndex(zm) => { import zm._ ; 
  //       val a : A = (tree : ZT.type#Tree[A])
  //       (a, () : N#Derivative[A])
  //     }
  //     case IsSuccIndex(sm) => { import sm._ ;
  //       (tree : STree[A]) match {
  //         case Cap() => Cap()
  //         case Joint(a, shell) => {

  //           val shellCorollas : PTree[STree[(A, N#Derivative[A])]] = 
  //             mapT[P, STree[A], STree[(A, N#Derivative[A])]](p, shell, 
  //               (t : STree[A]) => {
  //                 rewrite[({ type L[+X] = STree[(A, X)] })#L, ST[P]#Derivative[A], N#Derivative[A]](
  //                   zipWithCorolla[ST[P], A](ST(p), t)
  //                 )
  //               })

  //           val thisCorolla : N#Derivative[A] = 
  //             (const(p, Cap(), shell) : PTree[STree[A]] , Nil)

  //           Joint((a, thisCorolla), shellCorollas)

  //         }
  //       }
  //     }
  //   }

  // def zipWithPrefix[N <: TreeIndex, A](n : N, pref : N#Address, tree : N#Tree[A]) : N#Tree[(A, N#Address)] =
  //   n match {
  //     case IsZeroIndex(zm) => { import zm._ ;
  //       val a : A = (tree : ZT.type#Tree[A])
  //       (a , Nil)
  //     }
  //     case IsSuccIndex(sm) => { import sm._ ;
  //       (tree : STree[A]) match {
  //         case Cap() => Cap()
  //         case Joint(a, shell) => {

  //           val shellWithAddrs : PTree[(STree[A], P#Address)] =
  //             zipWithAddress(p, shell)

  //           val shellResult : PTree[STree[(A, N#Address)]] =
  //             mapT[P, (STree[A], P#Address), STree[(A, N#Address)]](p, shellWithAddrs, {
  //               case (st, ad) => {
  //                 rewrite[({ type L[X] = STree[(A, X)] })#L, ST[P]#Address, N#Address](
  //                   zipWithPrefix[ST[P], A](ST(p), ad :: (pref : ST[P]#Address), st)
  //                 )
  //               }
  //             })
              
  //           Joint((a, pref), shellResult)
  //         }
  //       }
  //     }
  //   }

  // def zipWithAddress[N <: TreeIndex, A](n : N, tree : N#Tree[A]) : N#Tree[(A, N#Address)] =
  //   zipWithPrefix(n, Nil, tree)

  // def flattenWithPrefix[N <: TreeIndex, A](
  //   n : N, 
  //   prefix : ST[N]#Address, 
  //   corolla : N#Derivative[ST[N]#Address], 
  //   tree : ST[N]#Tree[A]
  // ) : Option[N#Tree[ST[N]#Address]] = 
  //   n match {
  //     case IsZeroIndex(zm) => { import zm._ ; 
  //       tree match {
  //         case Cap() => Some(plug(n, corolla, prefix))
  //         case Joint(head, tail) => {
  //           val ocs = zm.succConversions
  //           import ocs.{treeCohEq => succTreeCohEq,
  //                       treeCoeEq => succTreeCoeEq,
  //                       addressCohEq => succAddressCohEq,
  //                       addressCoeEq => succAddresCoeEq}

  //           implicit val resultEq : _0#Tree[_1#Address] === N#Tree[ST[N]#Address] = 
  //             lift[Nothing, Nothing, Any, Any, _0#Tree, _1#Address, ST[N]#Address](
  //               implicitly[_1#Address === ST[N]#Address]
  //             ).andThen(
  //               implicitly[_0#Tree[ST[N]#Address] === N#Tree[ST[N]#Address]]
  //             )

  //           val tailRewrite : _0#Tree[_1#Tree[A]] = 
  //             rewrite[_0#Tree, ST[N]#Tree[A], _1#Tree[A]](tail)

  //           resultEq.subst[Option](
  //             flattenWithPrefix[_0, A](ZT, Nil :: prefix, (), tailRewrite)
  //           )
  //         }
  //       }
  //     }
  //     case IsSuccIndex(sm) => { import sm._ ; 
  //       tree match {
  //         case Cap() => Some(plug(n, corolla, prefix))
  //         case Joint(a, shell) => {
  //           if ((shell : STree[ST[N]#Tree[A]]).isCap) Some(Cap()) else {
  //             val ssm = sm.succConversions

  //             import ssm.{
  //               treeCohEq => succTreeCohEq,
  //               treeCoeEq => succTreeCoeEq,
  //               addressCohEq => succAddressCohEq,
  //               addressCoeEq => succAddresCoeEq
  //             }

  //             val graftShell : STree[(ST[ST[P]]#Address, ST[P]#Derivative[ST[ST[P]]#Address])] = 
  //               mapT[ST[P], 
  //                 ((ST[ST[P]]#Address, ST[P]#Derivative[ST[ST[P]]#Address]) , ST[P]#Address), 
  //                 (ST[ST[P]]#Address, ST[P]#Derivative[ST[ST[P]]#Address])
  //               ](ST(p), 
  //                 zipWithAddress[ST[P], (ST[ST[P]]#Address, ST[P]#Derivative[ST[ST[P]]#Address])](ST(p),
  //                   zipWithCorolla[ST[P], ST[ST[P]]#Address](ST(p),
  //                     rewrite[STree, ST[N]#Address, ST[ST[P]]#Address](
  //                       const(n, prefix, shell)
  //                     )
  //                   )
  //                 ),
  //                 { case ((at, d) , ah) => (ah :: at, d) }
  //               )

  //             for {
  //               zippedShells <- (
  //                 zipComplete[ST[P],
  //                   (ST[ST[P]]#Address, ST[P]#Derivative[ST[ST[P]]#Address]),
  //                   ST[ST[P]]#Tree[A]
  //                 ](ST(p), graftShell,
  //                   rewrite[STree, ST[N]#Tree[A], ST[ST[P]]#Tree[A]](shell)
  //                 )
  //               )

  //               flattenedShell <- (
  //                 sequenceT[ST[P], Option, STree[ST[ST[P]]#Address]](ST(p), 
  //                   mapT[ST[P],
  //                     ((ST[ST[P]]#Address, ST[P]#Derivative[ST[ST[P]]#Address]), ST[ST[P]]#Tree[A]),
  //                     Option[STree[ST[ST[P]]#Address]]
  //                   ](ST(p), zippedShells,
  //                     { case ((pr , cn) , tr) => flattenWithPrefix[ST[P], A](ST(p), pr, cn, tr) }
  //                   )
  //                 )
  //               )

  //               result <- substitute[ST[P], ST[ST[P]]#Address](ST(p), flattenedShell)

  //             } yield {
  //               rewrite[STree, ST[ST[P]]#Address, ST[N]#Address](result)
  //             }
  //           }
  //         }
  //       }
  //     }
  //   }

  // def flattenWithAddress[N <: TreeIndex, A](n : N, tree : ST[N]#Tree[A]) : Option[N#Tree[ST[N]#Address]] =
  //   flattenWithPrefix[N, A](n, Nil, globDeriv(n), tree)

  // def flatten[N <: TreeIndex, A](n : N, tree : ST[N]#Tree[A]) : Option[N#Tree[Unit]] =
  //   for {
  //     addrTree <- flattenWithAddress[N, A](n, tree)
  //   } yield shapeOf(n, addrTree)

  // def graft[N <: TreeIndex, A](n : N, tree : ST[N]#Tree[A], brs : N#Tree[ST[N]#Tree[A]]) : Option[ST[N]#Tree[A]] = {

  //   type Grafter[X] = StateT[Option, ST[N]#Tree[A], X]
  //   type GrafterS[S, X] = StateT[Option, S, X]
  //   type GrafterT[M[+_], X] = StateT[M, ST[N]#Tree[A], X]

  //   val GS = MonadState[GrafterS, ST[N]#Tree[A]]
  //   val GT = MonadTrans[GrafterT]

  //   import GS._
  //   import GT._

  //   for {
  //     addrTr <- flattenWithAddress[N, A](n, tree)
  //     grafts <- zipComplete[N, ST[N]#Address, ST[N]#Tree[A]](n, addrTr, brs)

  //     result <- sequenceT[N, Grafter, Unit](n,
  //       mapT[N,
  //         (ST[N]#Address, ST[N]#Tree[A]),
  //         Grafter[Unit]
  //       ](n, grafts, {
  //         case (addr, br) => {
  //           for {
  //             curTr <- get
  //             graftResult <- liftM[Option, ST[N]#Tree[A]](
  //               graftAt(n, addr, curTr, br)
  //             )
  //             _ <- put(graftResult)
  //           } yield ()
  //         }
  //       })
  //     ).exec(tree)

  //   } yield result
  // }

  // def graftAt[N <: TreeIndex, A](n : N, addr : ST[N]#Address, tree : ST[N]#Tree[A], br : ST[N]#Tree[A]) : Option[ST[N]#Tree[A]] =
  //   seek[ST[N], A](ST(n), addr, (tree, Nil)) match {
  //     case None => None
  //     case Some((Cap(), c)) => Some(close[ST[N], A](ST(n), c, br))
  //     case Some((Joint(_, _), _)) => None
  //   }

  // def substitute[N <: TreeIndex, A](n : N, ttree : N#Tree[N#Tree[A]]) : Option[N#Tree[A]] =
  //   n match {
  //     case IsZeroIndex(zm) => { import zm._ ; 
  //       val strip1 : ZT.type#Tree[N#Tree[A]] = ttree
  //       val strip2 : ZT.type#Tree[A] = strip1
  //       Some(strip2)
  //     }
  //     case IsSuccIndex(sm) => { import sm._ ; 
  //       (ttree : STree[N#Tree[A]]) match {
  //         case Cap() => Some(Cap())
  //         case Joint(t, sts) => {
  //           for {
  //             s <- sequenceT(p,
  //               mapT[P, STree[N#Tree[A]], Option[STree[A]]](p, sts,
  //                 (tt : STree[N#Tree[A]]) => {
  //                   rewrite[Option, N#Tree[A], STree[A]](
  //                     substitute[N, A](n, tt)
  //                   )
  //                 }
  //               )
  //             )

  //             g <- graft[P, A](p, t, s)

  //           } yield g
  //         }
  //       }
  //     }
  //   }

}
