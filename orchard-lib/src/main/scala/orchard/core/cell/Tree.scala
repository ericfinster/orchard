/**
  * Tree.scala - Higher Dimensional Trees
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.cell

import scala.language.higherKinds
import scala.language.implicitConversions

import scalaz._
import scalaz.Id._
import scalaz.std.option._

import Slice._

trait TreeFunctions {

  import TreeIndex._

  def map[N <: TreeIndex, A, B](n : N, tree : N#Tree[A], f : A => B) : N#Tree[B]
  def traverse[N <: TreeIndex, G[_], A, B](n : N, tree :  N#Tree[A], f : A => G[B])(implicit apG : Applicative[G]) : G[N#Tree[B]]

  def sequence[N <: TreeIndex, G[_], A](n : N, tree : N#Tree[G[A]])(implicit apG : Applicative[G]) : G[N#Tree[A]] =
    traverse(n, tree, identity[G[A]])

  def const[N <: TreeIndex, A, B](n : N, b : B, tree : N#Tree[A]) : N#Tree[B] =
    map[N, A, B](n, tree, (_ => b))

  def plug[N <: TreeIndex, A](n : N, d : N#Derivative[A], a : A) : N#Tree[A] =
    n match {
      case IsZeroIndex(zcs) => { import zcs._ ; a }
      case IsSuccIndex(scs) => { import scs._ ;
        (d : ST[P]#Derivative[A]) match {
          case (shell, context) => {
            close[ST[P], A](ST(p), context, Joint(a, shell))
          }
        }
      }
    }

  def close[N <: TreeIndex, A](n : N, c : N#Context[A], t : N#Tree[A]) : N#Tree[A] =
    n match {
      case IsZeroIndex(zcs) => { import zcs._ ; t }
      case IsSuccIndex(scs) => { import scs._ ;
        (c : ST[P]#Context[A]) match {
          case Nil => t
          case (a , d) :: cs => {
            close[ST[P], A](ST(p), cs, Joint(a , plug[P, STree[A]](p, d, t)))
          }
        }
      }
    }

  def visit[N <: TreeIndex, A](n : N, d : N#Direction, z : N#Zipper[A]) : Option[N#Zipper[A]] =
    n match {
      case IsZeroIndex(zcs) => None
      case IsOneIndex(ocs) => { import ocs._ ;

        val ud : ST[ZT.type]#Direction = d
        val uz : ST[ZT.type]#Zipper[A] = z

        (ud , uz) match {
          case (Nil, (focus , cntxt)) => {
            (focus : Slice[Id, A]) match {
              case Cap() => None
              case Joint(head, tail) => Some((tail, ((head, ()) :: cntxt)))
            }
          }
          case (_, _) => None
        }
      }
      case IsDblSuccIndex(dcs) => { import dcs._ ;

        val ud : ST[P]#Direction = d
        val uz : ST[P]#Zipper[A] = z

        (ud , uz) match {
          case (addr, (focus , cntxt)) => {
            (focus : Slice[FS, A]) match {
              case Cap() => None
              case Joint(a, shell) => {

                def visitBranch(zp : P#Zipper[ST[P]#Tree[A]]) : Option[N#Zipper[A]] =
                  zp match {
                    case (Cap(), _) => None
                    case (Joint(t, tsh), z0) => {
                      Some((t , (a , (tsh, z0)) :: cntxt) : N#Zipper[A])
                    }
                  }

                for {
                  shellContext <- seek[P, ST[P]#Tree[A]](p, addr, (shell, Nil))
                  zipAtBranch <- visitBranch(shellContext)
                } yield zipAtBranch
              }
            }
          }
        }
      }
    }

  def seek[N <: TreeIndex, A](n : N, a : N#Address, z : N#Zipper[A]) : Option[N#Zipper[A]] = 
    a match {
      case Nil => Some(z)
      case d :: ds => 
        for {
          zz <- seek(n, ds, z)
          zv <- visit(n, d, zz)
        } yield zv
    }


  def nGlob[N <: TreeIndex, A](n : N, a : A) : N#Tree[A] =
    n match {
      case IsZeroIndex(zcs) => { import zcs._ ; a }
      case IsSuccIndex(scs) => { import scs._ ; 
        Joint(a, nGlob[P, STree[A]](p, Cap())) 
      }
    }

  def globDeriv[N <: TreeIndex](n : N) : N#Derivative[ST[N]#Address] =
    n match {
      case IsZeroIndex(zcs) => { import zcs._ ; () }
      case IsSuccIndex(scs) => { import scs._ ;  
        (nGlob(p, nGlob(ST(p), Nil)), Nil)
      }
    }

  def zipComplete[N <: TreeIndex, A, B](n : N, ta : N#Tree[A], tb : N#Tree[B]) : Option[N#Tree[(A, B)]] =
    n match {
      case IsZeroIndex(zcs) => { import zcs._ ; 

        val a : ZT.type#Tree[A] = ta
        val b : ZT.type#Tree[B] = tb

        Some((a, b))

      }
      case IsSuccIndex(scs) => { import scs._ ;

        val sta : STree[A] = ta
        val stb : STree[B] = tb

        (sta, stb) match {
          case (Cap(), Cap()) => Some(Cap())
          case (Joint(a, ash), Joint(b, bsh)) => {

            for {
              branchPairs <- zipComplete[P, STree[A], STree[B]](p, ash, bsh)
              zippedShell <- sequence(p,
                (map[P, (STree[A], STree[B]), Option[STree[(A, B)]]](p, branchPairs, {
                  case (t1 : STree[A], t2 : STree[B]) => zipComplete[ST[P], A, B](ST(p), t1, t2)
                }))
              )
            } yield Joint((a, b), zippedShell)

          }
          case (_, _) => None
        }
      }     
    }

  def zipWithCorolla[N <: TreeIndex, A](n : N, tree : N#Tree[A]) : N#Tree[(A, N#Derivative[A])] =
    n match {
      case IsZeroIndex(zcs) => { import zcs._ ; 
        val a : A = (tree : ZT.type#Tree[A])
        (a, () : N#Derivative[A])
      }
      case IsSuccIndex(scs) => { import scs._ ;
        (tree : STree[A]) match {
          case Cap() => Cap()
          case Joint(a, shell) => {

            // The ugly cast here happens because we can't rewrite the type when 
            // it is captured by an outer constructor.  For this you need leibniz ...
            val shellCorollas : PTree[STree[(A, N#Derivative[A])]] = 
              map[P, STree[A], STree[(A, N#Derivative[A])]](p, shell, 
                (t : STree[A]) => zipWithCorolla[ST[P], A](ST(p), t).
                  asInstanceOf[STree[(A, N#Derivative[A])]]
              )

            val thisCorolla : N#Derivative[A] = 
              (const(p, Cap(), shell) : PTree[STree[A]] , Nil)

            Joint((a, thisCorolla), shellCorollas)

          }
        }
      }
    }

  def zipWithPrefix[N <: TreeIndex, A](n : N, pref : N#Address, tree : N#Tree[A]) : N#Tree[(A, N#Address)] =
    n match {
      case IsZeroIndex(zcs) => { import zcs._ ;
        val a : A = (tree : ZT.type#Tree[A])
        (a , Nil)
      }
      case IsSuccIndex(scs) => { import scs._ ;
        (tree : STree[A]) match {
          case Cap() => Cap()
          case Joint(a, shell) => {

            val shellWithAddrs : PTree[(STree[A], P#Address)] =
              zipWithAddress(p, shell)

            // Another ugly cast ....
            val shellResult : PTree[STree[(A, N#Address)]] =
              map[P, (STree[A], P#Address), STree[(A, N#Address)]](p, shellWithAddrs, {
                case (st, ad) => zipWithPrefix[ST[P], A](ST(p), ad :: (pref : ST[P]#Address), st).
                    asInstanceOf[STree[(A, N#Address)]]
              })
              
            Joint((a, pref), shellResult)
          }
        }
      }
    }

  def zipWithAddress[N <: TreeIndex, A](n : N, tree : N#Tree[A]) : N#Tree[(A, N#Address)] =
    zipWithPrefix(n, Nil, tree)


  def flattenWithPref[N <: TreeIndex, A](
    n : N, 
    prefix : ST[N]#Address, 
    corolla : N#Derivative[ST[N]#Address], 
    tree : ST[N]#Tree[A]
  ) : Option[N#Tree[ST[N]#Address]]

  def flattenWithAddr[N <: TreeIndex, A](n : N, tree : ST[N]#Tree[A]) : Option[N#Tree[ST[N]#Address]] =
    flattenWithPref[N, A](n, Nil, globDeriv(n), tree)

  def graft[N <: TreeIndex, A](n : N, tree : ST[N]#Tree[A], brs : N#Tree[ST[N]#Tree[A]]) : Option[ST[N]#Tree[A]]
  def graftAt[N <: TreeIndex, A](n : N, addr : ST[N]#Address, tree : ST[N]#Tree[A], br : ST[N]#Tree[A]) : Option[ST[N]#Tree[A]]

  def substitute[N <: TreeIndex, A](n : N, ttree : N#Tree[N#Tree[A]]) : Option[N#Tree[A]]

}
