/**
  * TreeFunctions.scala - Functions on trees depending on dimension
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import scala.language.higherKinds

import scalaz.{Tree => _, Zipper => _, _}
import scalaz.std.option._

import Nats._
import Trees._
import Slice._

trait TreeFunctions[N <: Nat] {

  implicit val theseFunctions : TreeFunctions[N] = this

  def map[A, B](tree : Tree[N, A], f : A => B) : Tree[N, B]
  def traverse[G[_], A, B](tree :  Tree[N, A], f : A => G[B])(implicit apG : Applicative[G]) : G[Tree[N, B]]
  def sequence[G[_], A](tree : Tree[N, G[A]])(implicit apG : Applicative[G]) : G[Tree[N, A]] =
    traverse(tree, identity[G[A]])

  def isLeaf[A](tree : Tree[N, A]) : Boolean
  def isBranch[A](tree : Tree[N, A]) : Boolean

  def const[A, B](b : B, tree : Tree[N, A]) : Tree[N, B] = map(tree, ((a : A) => b))    
  def shapeOf[A](tree : Tree[N, A]) : Tree[N, Unit] = const((), tree)

  def plug[A](d : Derivative[N, A], a : A) : Tree[N, A] 
  def close[A](c : Context[N, A], t : Tree[N, A]) : Tree[N, A]

  def visit[A](d : Direction[N], z : Zipper[N, A]) : Option[Zipper[N, A]] 

  def seek[A](a : Address[N], z : Zipper[N, A]) : Option[Zipper[N, A]] =
    a match {
      case Nil => Some(z)
      case d :: ds => 
        for {
          zz <- seek(ds, z)
          zzz <- visit(d, zz)
        } yield zzz
    }

  def emptyContext[A] : Context[N, A]
  def rootZipper[A](tree : Tree[N, A]) : Zipper[N, A] =
    (tree, emptyContext)

  def nGlob[A](a : A) : Tree[N, A] 
  def globDeriv : Derivative[N, Address[S[N]]]

  def zipComplete[A, B](ta : Tree[N, A], tb : Tree[N, B]) : Option[Tree[N, (A, B)]]
  def zipWithCorolla[A](tree : Tree[N, A]) : Tree[N, (A, Derivative[N, A])]
  def zipWithPrefix[A](pref : Address[N], tree : Tree[N, A]) : Tree[N, (A, Address[N])]
  def zipWithAddress[A](tree : Tree[N, A]) : Tree[N, (A, Address[N])] =
    zipWithPrefix(Nil, tree)

  def flattenWithPrefix[A](
    prefix : Address[S[N]],
    corolla : Derivative[N, Address[S[N]]],
    tree : Tree[S[N], A]
  ) : Option[Tree[N, Address[S[N]]]]

  def flattenWithAddress[A](tree : Tree[S[N], A]) : Option[Tree[N, Address[S[N]]]] =
    flattenWithPrefix(Nil, globDeriv, tree)

  def flatten[A](tree : Tree[S[N], A]) : Option[Tree[N, Unit]] = 
    for {
      addrTree <- flattenWithAddress(tree)
    } yield shapeOf(addrTree)

  def substitute[A](ttree : Tree[N, Tree[N, A]]) : Option[Tree[N, A]]

  def graft[A](tree : Tree[S[N], A], brs : Tree[N, Tree[S[N], A]]) : Option[Tree[S[N], A]] = {

    type Grafter[X] = StateT[Option, Tree[S[N], A], X]
    type GrafterS[S, X] = StateT[Option, S, X]
    type GrafterT[M[+_], X] = StateT[M, Tree[S[N], A], X]

    val GS = MonadState[GrafterS, Tree[S[N], A]]
    val GT = MonadTrans[GrafterT]

    import GS.{map => _, sequence => _, _}
    import GT._

    for {
      addrTr <- flattenWithAddress(tree)
      grafts <- zipComplete(addrTr, brs)

      result <- sequence[Grafter, Unit](
        grafts map {
          case (addr, br) => {
            for {
              curTr <- get
              graftResult <- liftM[Option, Tree[S[N], A]](
                graftAt(addr, curTr, br)
              )
              _ <- put(graftResult)
            } yield ()
          }
        }
      ).exec(tree)

    } yield result


  }

  def graftAt[A](addr : Address[S[N]], tree : Tree[S[N], A], br : Tree[S[N], A]) : Option[Tree[S[N], A]] =
    for {
      z <- tree seek addr

      result <- (
        z match {
          case (Leaf(), context) => {
            val sfns = implicitly[TreeFunctions[S[N]]]
            Some(sfns.close(context, br))
          }
          case (_, _) => None
        }
      )
    } yield result

}

object TreeZeroFunctions extends TreeFunctions[_0] {

  def map[A, B](tree : Tree[_0, A], f : A => B) : Tree[_0, B] = 
    tree match {
      case Point(a) => Point(f(a))
    }

  def traverse[G[_], A, B](tree :  Tree[_0, A], f : A => G[B])(implicit apG : Applicative[G]) : G[Tree[_0, B]] = 
    tree match {
      case Point(a) => { import apG._ ; ap(f(a))(pure((b : B) => Point(b))) }
    }

  def isLeaf[A](tree : Tree[_0, A]) : Boolean = false
  def isBranch[A](tree : Tree[_0, A]) : Boolean = true

  def plug[A](d : Derivative[_0, A], a : A) : Tree[_0, A] = Point(a)
  def close[A](c : Context[_0, A], t : Tree[_0, A]) : Tree[_0, A] = t

  def visit[A](d : Direction[_0], z : Zipper[_0, A]) : Option[Zipper[_0, A]] = None
  def emptyContext[A] : Context[_0, A] = ()

  def nGlob[A](a : A) : Tree[_0, A] = Point(a)
  def globDeriv : Derivative[_0, Address[_1]] = ()

  def zipComplete[A, B](ta : Tree[_0, A], tb : Tree[_0, B]) : Option[Tree[_0, (A, B)]] =
    (ta, tb) match {
      case (Point(a), Point(b)) => Some(Point((a, b)))
    }

  def zipWithCorolla[A](tree : Tree[_0, A]) : Tree[_0, (A, Derivative[_0, A])] = 
    tree match {
      case Point(a) => Point(a, ())
    }

  def zipWithPrefix[A](pref : Address[_0], tree : Tree[_0, A]) : Tree[_0, (A, Address[_0])] = 
    tree match {
      case Point(a) => Point(a, Nil)
    }

  def flattenWithPrefix[A](
    prefix : Address[_1],
    corolla : Derivative[_0, Address[_1]],
    tree : Tree[_1, A]
  ) : Option[Tree[_0, Address[_1]]] =
    tree match {
      case Leaf() => Some(plug(corolla, prefix))
      case Branch(head, Point(tail)) => flattenWithPrefix(Nil :: prefix, (), tail)
    }

  def substitute[A](ttree : Tree[_0, Tree[_0, A]]) : Option[Tree[_0, A]] =
    ttree match {
      case Point(Point(a)) => Some(Point(a))
    }

}

abstract class TreeSuccFunctions[N <: Nat] extends TreeFunctions[S[N]] {

  val prev : TreeFunctions[N]

  def map[A, B](tree : Tree[S[N], A], f : A => B) : Tree[S[N], B] = {
    tree match {
      case Leaf() => Leaf()
      case Branch(a, shell) => 
        Branch(f(a), prev.map(shell, (t : Tree[S[N], A]) => map(t, f)))
    }
  }

  def traverse[G[_], A, B](tree :  Tree[S[N], A], f : A => G[B])(implicit apG : Applicative[G]) : G[Tree[S[N], B]] = {

    import apG._

    tree match {
      case Leaf() => pure(Leaf())
      case Branch(a, shell) => {

        val branchCons : G[(B, Tree[N, Tree[S[N], B]]) => Tree[S[N], B]] =
          pure((b : B, t : Tree[N, Tree[S[N], B]]) => Branch(b, t))

        val traversedShell : G[Tree[N, Tree[S[N], B]]] = 
          prev.sequence(
            prev.map(shell, (t : Tree[S[N], A]) => this.traverse(t, f))
          )

        ap2(f(a), traversedShell)(branchCons)

      }
    }

  }

  def isLeaf[A](tree : Tree[S[N], A]) : Boolean = 
    tree match {
      case Leaf() => true
      case _ => false
    }

  def isBranch[A](tree : Tree[S[N], A]) : Boolean =
    tree match {
      case Branch(a, shell) => true
      case _ => false
    }

  def plug[A](d : Derivative[S[N], A], a : A) : Tree[S[N], A] =
    d match {
      case (shell, context) => close(context, Branch(a, shell))
    }

  def close[A](c : Context[S[N], A], t : Tree[S[N], A]) : Tree[S[N], A] =
    c match {
      case Nil => t
      case (a, d) :: cs => close(cs, Branch(a, prev.plug(d, t)))
    }

  def visit[A](d : Direction[S[N]], z : Zipper[S[N], A]) : Option[Zipper[S[N], A]] 

  def emptyContext[A] : Context[S[N], A] = Nil

  def nGlob[A](a : A) : Tree[S[N], A] = Branch(a, prev.nGlob(Leaf()))
  def globDeriv : Derivative[S[N], Address[S[S[N]]]] = (prev.nGlob(Leaf()), Nil)

  def zipComplete[A, B](ta : Tree[S[N], A], tb : Tree[S[N], B]) : Option[Tree[S[N], (A, B)]] = 
    (ta, tb) match {
      case (Leaf(), Leaf()) => Some(Leaf())
      case (Branch(a, ash), Branch(b, bsh)) => {

        for {
          zippedShells <- prev.zipComplete(ash, bsh)

          resultShell <- prev.sequence(
            prev.map(zippedShells, (tpr : (Tree[S[N], A], Tree[S[N], B])) => {
              zipComplete(tpr._1, tpr._2)
            })
          )

        } yield Branch((a, b), resultShell)

      }
    }

  def zipWithCorolla[A](tree : Tree[S[N], A]) : Tree[S[N], (A, Derivative[S[N], A])] = 
    tree match {
      case Leaf() => Leaf()
      case Branch(a, shell) => 
        Branch((a, (prev.const(Leaf(), shell), Nil)), 
          prev.map(shell, (t : Tree[S[N], A]) => { zipWithCorolla(t) }))
    }

  def zipWithPrefix[A](pref : Address[S[N]], tree : Tree[S[N], A]) : Tree[S[N], (A, Address[S[N]])] =
    tree match {
      case Leaf() => Leaf()
      case Branch(a, shell) => 
        Branch((a, pref), prev.map(prev.zipWithAddress(shell), 
          (apr : (Tree[S[N], A], Address[N])) => {
            zipWithPrefix(apr._2 :: pref, apr._1)
          }))
    }

  def flattenWithPrefix[A](
    prefix : Address[S[S[N]]],
    corolla : Derivative[S[N], Address[S[S[N]]]],
    tree : Tree[S[S[N]], A]
  ) : Option[Tree[S[N], Address[S[S[N]]]]] = 
    tree match {
      case Leaf() => Some(plug(corolla, prefix))
      case Branch(a, shell) => 
        if (isLeaf(shell)) Some(Leaf()) else {

          val graftShell : Tree[S[N], (Address[S[S[N]]], Derivative[S[N], Address[S[S[N]]]])] = 
            zipWithAddress(zipWithCorolla(const(prefix, shell))) map {
              case ((at, d), ah) => (ah :: at, d)
            }

          for {
            zt <- zipComplete(graftShell, shell)

            toSubst <- sequence(
              zt map {
                case ((pr, cr), tr) => flattenWithPrefix(pr, cr, tr)
              }
            )

            result <- substitute(toSubst)

          } yield result

        }
    }

  def substitute[A](ttree : Tree[S[N], Tree[S[N], A]]) : Option[Tree[S[N], A]] = 
    ttree match {
      case Leaf() => Some(Leaf())
      case Branch(t, sts) => 
        for {
          s <- prev.sequence(
            prev.map(sts, (tt : Tree[S[N], Tree[S[N], A]]) => {
              substitute(tt)
            })
          )

          g <- prev.graft(t, s)

        } yield g
    }
}
