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

sealed abstract class Dir[N <: Nat] { def dim : N }
case class Root[N <: Nat](implicit val p : N) extends Dir[S[N]] { def dim = S(p) }
case class Step[N <: Nat](d : Dir[N], ds : Dir[S[N]]) extends Dir[S[N]] { def dim = ds.dim }

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

case class Leaf[N <: Nat](addr : Dir[S[N]]) extends Tree[S[N], Nothing] {

  def dim = addr.dim

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

trait TreeFunctions {

  //============================================================================================
  // GRAFT
  //

  def graft[N <: Nat, A](tr : Tree[S[N], A], brs : Tree[N, Tree[S[N], A]]) : Option[Tree[S[N], A]] = 
    tr match {
      case Leaf(addr) => brs valueAt addr
      case Node(a, sh) =>
        for {
          nsh <- sh.traverse(graft(_, brs))
        } yield Node(a, nsh)
    }


  //============================================================================================
  // JOIN
  //

  def join[N <: Nat, A](tr : Tree[N, Tree[N, A]]) : Option[Tree[N, A]] =  
    natParamRec(tr.dim)(JoinRecursor)(tr)

  type JoinIn[M <: Nat, A] = Tree[M, Tree[M, A]]
  type JoinOut[M <: Nat, A] = Option[Tree[M, A]]

  object JoinRecursor extends NatParamRecursor[JoinIn, JoinOut] {

    def caseZero[A](tr : Tree[_0, Tree[_0, A]]) : Option[Tree[_0, A]] = 
      tr.rootValue

    def caseSucc[P <: Nat, A](tr : Tree[S[P], Tree[S[P], A]]) : Option[Tree[S[P], A]] =
      tr match {
        case Leaf(addr) => Some(Leaf(addr))
        case Node(t, tsh) =>
          for {
            gsh <- tsh.traverse(caseSucc(_))
            str <- graft(t, gsh)
          } yield str
      }

  }

  //============================================================================================
  // UNZIP
  //

  def unzip[N <: Nat, A, B](tr : Tree[N, (A, B)]) : (Tree[N, A], Tree[N, B]) = 
    natTwoParamRec(tr.dim)(UnzipRecursor)(tr)

  type UnzipIn[M <: Nat, A, B] = Tree[M, (A, B)]
  type UnzipOut[M <: Nat, A, B] = (Tree[M, A], Tree[M, B])

  object UnzipRecursor extends NatTwoParamRecursor[UnzipIn, UnzipOut] {

    def caseZero[A, B](tr : Tree[_0, (A, B)]) : (Tree[_0, A], Tree[_0, B]) = 
      tr match {
        case Pt((a, b)) => (Pt(a), Pt(b))
      }

    def caseSucc[P <: Nat, A, B](tr : Tree[S[P], (A, B)]) : (Tree[S[P], A], Tree[S[P], B]) = 
      tr match {
        case Leaf(addr) => (Leaf(addr), Leaf(addr))
        case Node((a, b), shell) => {
          val (ash, bsh) = unzip(shell map (unzip(_)))
          (Node(a, ash), Node(b, bsh))
        }
      }

  }

}

object Tree extends TreeFunctions {

  // Probably can find a better place for this ...
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

}
