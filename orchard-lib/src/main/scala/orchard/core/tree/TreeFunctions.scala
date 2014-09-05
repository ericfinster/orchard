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

trait TreeFunctions[N <: Nat] {

  implicit val theseFunctions : TreeFunctions[N] = this

  def map[A, B](tree : Tree[N, A], f : A => B) : Tree[N, B]
  def traverse[G[_], A, B](tree :  Tree[N, A], f : A => G[B])(implicit apG : Applicative[G]) : G[Tree[N, B]]
  def sequence[G[_], A](tree : Tree[N, G[A]])(implicit apG : Applicative[G]) : G[Tree[N, A]]

  def const[A, B](b : B, tree : Tree[N, A]) : Tree[N, B]
  def shapeOf[A](tree : Tree[N, A]) : Tree[N, Unit]

  def plug[A](d : Derivative[N, A], a : A) : Tree[N, A] 
  def close[A](c : Context[N, A], t : Tree[N, A]) : Tree[N, A]

  def visit[A](d : Direction[N], z : Zipper[N, A]) : Option[Zipper[N, A]] 
  def seek[A](a : Address[N], z : Zipper[N, A]) : Option[Zipper[N, A]] 
  def emptyContext[A] : Context[N, A]

  def nGlob[A](a : A) : Tree[N, A] 
  def globDeriv : Derivative[N, Address[S[N]]]

  def zipComplete[A, B](ta : Tree[N, A], tb : Tree[N, B]) : Option[Tree[N, (A, B)]]
  def zipWithCorolla[A](tree : Tree[N, A]) : Tree[N, (A, Derivative[N, A])]
  def zipWithPrefix[A](pref : Address[N], tree : Tree[N, A]) : Tree[N, (A, Address[N])]
  def zipWithAddress[A](tree : Tree[N, A]) : Tree[N, (A, Address[N])]

  def flattenWithPrefix[A](
    prefix : Address[S[N]],
    corolla : Derivative[N, Address[S[N]]],
    tree : Tree[S[N], A]
  ) : Option[Tree[N, Address[S[N]]]]

  def flattenWithAddress[A](tree : Tree[S[N], A]) : Option[Tree[N, Address[S[N]]]]
  def flatten[A](tree : Tree[S[N], A]) : Option[Tree[N, Unit]]
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
          case (Leaf(), context) => ???
          case (_, _) => None
        }
      )
    } yield result

}

object TreeZeroFunctions extends TreeFunctions[_0] {

  def map[A, B](tree : Tree[_0, A], f : A => B) : Tree[_0, B] = ???
  def traverse[G[_], A, B](tree :  Tree[_0, A], f : A => G[B])(implicit apG : Applicative[G]) : G[Tree[_0, B]] = ???
  def sequence[G[_], A](tree : Tree[_0, G[A]])(implicit apG : Applicative[G]) : G[Tree[_0, A]] = ???

  def const[A, B](b : B, tree : Tree[_0, A]) : Tree[_0, B] = ???
  def shapeOf[A](tree : Tree[_0, A]) : Tree[_0, Unit] = ???

  def plug[A](d : Derivative[_0, A], a : A) : Tree[_0, A] = ???
  def close[A](c : Context[_0, A], t : Tree[_0, A]) : Tree[_0, A] = ???

  def visit[A](d : Direction[_0], z : Zipper[_0, A]) : Option[Zipper[_0, A]] = ???
  def seek[A](a : Address[_0], z : Zipper[_0, A]) : Option[Zipper[_0, A]] = ???
  def emptyContext[A] : Context[_0, A] = ()

  def nGlob[A](a : A) : Tree[_0, A] = ???
  def globDeriv : Derivative[_0, Address[_1]] = ???

  def zipComplete[A, B](ta : Tree[_0, A], tb : Tree[_0, B]) : Option[Tree[_0, (A, B)]] = ???
  def zipWithCorolla[A](tree : Tree[_0, A]) : Tree[_0, (A, Derivative[_0, A])] = ???
  def zipWithPrefix[A](pref : Address[_0], tree : Tree[_0, A]) : Tree[_0, (A, Address[_0])] = ???
  def zipWithAddress[A](tree : Tree[_0, A]) : Tree[_0, (A, Address[_0])] = ???

  def flattenWithPrefix[A](
    prefix : Address[_1],
    corolla : Derivative[_0, Address[_1]],
    tree : Tree[_1, A]
  ) : Option[Tree[_0, Address[_1]]] = ???

  def flattenWithAddress[A](tree : Tree[_1, A]) : Option[Tree[_0, Address[_1]]] = ???
  def flatten[A](tree : Tree[_1, A]) : Option[Tree[_0, Unit]] = ???
  def substitute[A](ttree : Tree[_0, Tree[_0, A]]) : Option[Tree[_0, A]] = ???

}

abstract class TreeSuccFunctions[N <: Nat] extends TreeFunctions[S[N]] {

  val prev : TreeFunctions[N]

  def map[A, B](tree : Tree[S[N], A], f : A => B) : Tree[S[N], B]
  def traverse[G[_], A, B](tree :  Tree[S[N], A], f : A => G[B])(implicit apG : Applicative[G]) : G[Tree[S[N], B]]
  def sequence[G[_], A](tree : Tree[S[N], G[A]])(implicit apG : Applicative[G]) : G[Tree[S[N], A]]

  def const[A, B](b : B, tree : Tree[S[N], A]) : Tree[S[N], B]
  def shapeOf[A](tree : Tree[S[N], A]) : Tree[S[N], Unit]

  def plug[A](d : Derivative[S[N], A], a : A) : Tree[S[N], A] 
  def close[A](c : Context[S[N], A], t : Tree[S[N], A]) : Tree[S[N], A]

  def visit[A](d : Direction[S[N]], z : Zipper[S[N], A]) : Option[Zipper[S[N], A]] 
  def seek[A](a : Address[S[N]], z : Zipper[S[N], A]) : Option[Zipper[S[N], A]] 
  def emptyContext[A] : Context[S[N], A] = Nil

  def nGlob[A](a : A) : Tree[S[N], A] 
  def globDeriv : Derivative[S[N], Address[S[S[N]]]]

  def zipComplete[A, B](ta : Tree[S[N], A], tb : Tree[S[N], B]) : Option[Tree[S[N], (A, B)]]
  def zipWithCorolla[A](tree : Tree[S[N], A]) : Tree[S[N], (A, Derivative[S[N], A])]
  def zipWithPrefix[A](pref : Address[S[N]], tree : Tree[S[N], A]) : Tree[S[N], (A, Address[S[N]])]
  def zipWithAddress[A](tree : Tree[S[N], A]) : Tree[S[N], (A, Address[S[N]])]

  def flattenWithPrefix[A](
    prefix : Address[S[S[N]]],
    corolla : Derivative[S[N], Address[S[S[N]]]],
    tree : Tree[S[S[N]], A]
  ) : Option[Tree[S[N], Address[S[S[N]]]]]

  def flattenWithAddress[A](tree : Tree[S[S[N]], A]) : Option[Tree[S[N], Address[S[S[N]]]]]
  def flatten[A](tree : Tree[S[S[N]], A]) : Option[Tree[S[N], Unit]]
  def graft[A](tree : Tree[S[S[N]], A], brs : Tree[S[N], Tree[S[S[N]], A]]) : Option[Tree[S[S[N]], A]]



  def graftAt[A](addr : Address[S[S[N]]], tree : Tree[S[S[N]], A], br : Tree[S[S[N]], A]) : Option[Tree[S[S[N]], A]]

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
