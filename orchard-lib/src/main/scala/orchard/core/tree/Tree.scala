/**
  * Tree.scala - Higher Dimensional Trees
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import scala.language.higherKinds
import scala.language.implicitConversions
import scalaz.{Tree => _, Zipper => _, _}
import scalaz.std.option._
import scalaz.Leibniz._

import Nats._

sealed abstract class Dir[N <: Nat]
case class Root[N <: Nat]() extends Dir[S[N]]
case class Step[N <: Nat](d : Dir[N], ds : Dir[S[N]]) extends Dir[S[N]]

sealed abstract class Tree[N <: Nat, +A] {

  import Tree._

  val T = treeIsTraverse[N]

  def dim : N

  def map[B](f : A => B) : Tree[N, B]
  def traverse[G[_], B](f : A => G[B])(implicit apG : Applicative[G]) : G[Tree[N, B]] 

  def rootValue : Option[A]
  def valueAt(addr : Addr[N]) : Option[A] = 
    for {
      zp <- this seek addr
      a <- zp.focus.rootValue
    } yield a

  def isLeaf : Boolean
  def isNode : Boolean

  def nodes : List[A] = T.toList(this)
  def nodeCount : Int = T.count(this)
  def zipWithIndex : (Int, Tree[N, (A, Int)]) = 
    T.mapAccumL(this, 0)((i : Int, a : A) => (i + 1, (a, i)))

  def matchWith[B](tr : Tree[N, B]) : Option[Tree[N, (A, B)]]

}

case class Pt[+A](a : A) extends Tree[_0, A] {

  def dim = Z

  def map[B](f : A => B) : Tree[_0, B] = Pt(f(a))
  def traverse[G[_], B](f : A => G[B])(implicit apG : Applicative[G]) : G[Tree[_0, B]] = { 
    import apG._ 
    ap(f(a))(pure((b : B) => Pt(b))) 
  }

  def rootValue : Option[A] = Some(a)

  def matchWith[B](tr : Tree[_0, B]) : Option[Tree[_0, (A, B)]] = 
    tr match {
      case Pt(b) => Some(Pt((a, b)))
    }

  def isLeaf : Boolean = false
  def isNode : Boolean = true

}

case class Leaf[N <: Nat](addr : Dir[S[N]])(implicit val p : N) extends Tree[S[N], Nothing] {

  def dim = S(p)

  def map[B](f : Nothing => B) : Tree[S[N], Nothing] = this
  def traverse[G[_], B](f : Nothing => G[B])(implicit apG : Applicative[G]) : G[Tree[S[N], B]] = {
    import apG._
    pure(this)
  }

  def rootValue : Option[Nothing] = None

  def isLeaf : Boolean = true
  def isNode : Boolean = false

  def matchWith[B](tr : Tree[S[N], B]) : Option[Tree[S[N], (Nothing, B)]] = 
    if (tr.isLeaf) Some(this) else None

}

case class Node[N <: Nat, +A](a : A, shell : Tree[N, Tree[S[N], A]]) extends Tree[S[N], A] {

  def dim = S(shell.dim)

  def map[B](f : A => B) : Tree[S[N], B] = Node(f(a), shell map (_.map(f)))
  def traverse[G[_], B](f : A => G[B])(implicit apG : Applicative[G]) : G[Tree[S[N], B]] = {
    import apG._

    val nodeCons : G[(B , Tree[N, Tree[S[N], B]]) => Tree[S[N], B]] =
      pure((b : B, sh : Tree[N, Tree[S[N], B]]) => Node(b, sh))

    ap2(f(a), shell.traverse(_.traverse(f)))(nodeCons)

  }

  def rootValue : Option[A] = Some(a)

  def isLeaf : Boolean = false
  def isNode : Boolean = true

  def matchWith[B](tr : Tree[S[N], B]) : Option[Tree[S[N], (A, B)]] =
    tr match {
      case Leaf(_) => None
      case Node(b, bshell) => 
        for {
          msh <- shell matchWith bshell
          psh <- msh traverse {
            case (at, bt) => at matchWith bt
          }
        } yield Node((a, b), psh)
    }

}

object Tree {

  type Addr[N <: Nat] = Dir[S[N]]

  implicit def asRootZipper[N <: Nat, A](tr : Tree[N, A]) : Zipper[N, A] = 
    tr.dim match {
      case IsZero(zm) => {
        import zm._
        val zp = FocusPoint(zeroCoh.subst[({ type L[M <: Nat] = Tree[M, A] })#L](tr))
        zeroCoe.subst[({ type L[M <: Nat] = Zipper[M, A] })#L](zp)
      }
      case IsOne(om) => {
        import om._
        val zp = FocusList(oneCoh.subst[({ type L[M <: Nat] = Tree[M, A] })#L](tr), Empty())
        oneCoe.subst[({ type L[M <: Nat] = Zipper[M, A] })#L](zp)
      }
      case IsDblSucc(dm) => {
        import dm._
        val zp = FocusBranch(dblSuccCoh.subst[({ type L[M <: Nat] = Tree[M, A] })#L](tr), Empty[S[S[PP]]]())
        dblSuccCoe.subst[({ type L[M <: Nat] = Zipper[M, A] })#L](zp)
      }
    }

  implicit def treeIsTraverse[N <: Nat] : Traverse[({ type L[+A] = Tree[N, A] })#L] = 
    new Traverse[({ type L[+A] = Tree[N, A] })#L] {

      def traverseImpl[G[_], A, B](ta : Tree[N, A])(f : A => G[B])(implicit isA : Applicative[G]) : G[Tree[N, B]] = 
        ta.traverse(f)

    }

  // It would somehow be more reasonable to have these use unapply methods to detect that the label
  // type is of the appropriate shape.  Then they could be put into operations classes instead of 
  // living here, kind of floating in the companion object.

  def graft[N <: Nat, A](tr : Tree[S[N], A], brs : Tree[N, Tree[S[N], A]]) : Option[Tree[S[N], A]] = 
    tr match {
      case Leaf(addr) => brs valueAt addr
      case Node(a, sh) =>
        for {
          nsh <- sh.traverse(graft(_, brs))
        } yield Node(a, nsh)
    }


  def join[N <: Nat, A](tr : Tree[N, Tree[N, A]]) : Option[Tree[N, A]] =  {

    def zeroJoin[A](tr : Tree[_0, Tree[_0, A]]) : Option[Tree[_0, A]] =
      tr.rootValue

    def succJoin[N <: Nat, A](tr : Tree[S[N], Tree[S[N], A]]) : Option[Tree[S[N], A]] =
      tr match {
        case l @ Leaf(addr) => Some(Leaf(addr)(l.p))
        case Node(t, tsh) =>
          for {
            gsh <- tsh.traverse(succJoin(_))
            str <- graft(t, gsh)
          } yield str
      }

    tr.dim match {
      case IsZero(zm) => {
        import zm._
        val zt : Tree[_0, Tree[_0, A]] = 
          zeroCoh.subst[({ type L[M <: Nat] = Tree[M, Tree[M, A]] })#L](tr)
        zeroCoe.subst[({ type L[M <: Nat] = Option[Tree[M, A]] })#L](zeroJoin(zt))
      }
      case IsSucc(sm) => {
        import sm._
        val st : Tree[S[P], Tree[S[P], A]] = 
          succCoh.subst[({ type L[M <: Nat] = Tree[M, Tree[M, A]] })#L](tr)
        succCoe.subst[({ type L[M <: Nat] = Option[Tree[M, A]] })#L](succJoin(st))
      }
    }

  }

  def unzip[N <: Nat, A, B](tr : Tree[N, (A, B)]) : (Tree[N, A], Tree[N, B]) = ???

}
