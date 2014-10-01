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

  def dim : N

  def map[A, B](tree : Tree[N, A], f : A => B) : Tree[N, B]
  def traverse[G[_], A, B](tree :  Tree[N, A], f : A => G[B])(implicit apG : Applicative[G]) : G[Tree[N, B]]

  def isLeaf[A](tree : Tree[N, A]) : Boolean
  def isNode[A](tree : Tree[N, A]) : Boolean

  def plug[A](d : Derivative[N, A], a : A) : Tree[N, A] 
  def close[A](c : Context[N, A], t : Tree[N, A]) : Tree[N, A]

  def emptyContext[A] : Context[N, A]

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

  def value[A](tree : Tree[N, A]) : Option[A]
  def valueAt[A](tree : Tree[N, A], addr : Address[N]) : Option[A]

  def zipComplete[A, B](ta : Tree[N, A], tb : Tree[N, B]) : Option[Tree[N, (A, B)]]
  def zipWithPrefix[A](pref : Address[N], tree : Tree[N, A]) : Tree[N, (A, Address[N])]
  def zipWithAddress[A](tree : Tree[N, A]) : Tree[N, (A, Address[N])] =
    zipWithPrefix(Nil, tree)

  def graft[A](tree : Tree[S[N], A], brs : Tree[N, Tree[S[N], A]]) : Option[Tree[S[N], A]] 
  def substitute[A](ttree : Tree[N, Tree[N, A]]) : Option[Tree[N, A]]

  def fromXml[A](nd : xml.Node)(implicit xmlRd : XmlReadable[A]) : Tree[N, A]

}

object TreeZeroFunctions extends TreeFunctions[_0] {

  val dim = Z

  def map[A, B](tree : Tree[_0, A], f : A => B) : Tree[_0, B] = 
    tree match {
      case Point(a) => Point(f(a))
    }

  def traverse[G[_], A, B](tree :  Tree[_0, A], f : A => G[B])(implicit apG : Applicative[G]) : G[Tree[_0, B]] = 
    tree match {
      case Point(a) => { import apG._ ; ap(f(a))(pure((b : B) => Point(b))) }
    }

  def isLeaf[A](tree : Tree[_0, A]) : Boolean = false
  def isNode[A](tree : Tree[_0, A]) : Boolean = true

  def plug[A](d : Derivative[_0, A], a : A) : Tree[_0, A] = Point(a)
  def close[A](c : Context[_0, A], t : Tree[_0, A]) : Tree[_0, A] = t

  def emptyContext[A] : Context[_0, A] = ()

  def visit[A](d : Direction[_0], z : Zipper[_0, A]) : Option[Zipper[_0, A]] = None

  def value[A](tree : Tree[_0, A]) : Option[A] = 
    tree match {
      case Point(a) => Some(a)
    }

  def valueAt[A](tree : Tree[_0, A], addr : Address[_0]) : Option[A] = 
    value(tree)

  def zipComplete[A, B](ta : Tree[_0, A], tb : Tree[_0, B]) : Option[Tree[_0, (A, B)]] =
    (ta, tb) match {
      case (Point(a), Point(b)) => Some(Point((a, b)))
    }

  def zipWithPrefix[A](pref : Address[_0], tree : Tree[_0, A]) : Tree[_0, (A, Address[_0])] = 
    tree match {
      case Point(a) => Point(a, Nil)
    }


  def graft[A](tree : Tree[_1, A], brs : Tree[_0, Tree[_1, A]]) : Option[Tree[_1, A]] = 
    (tree, brs) match {
      case (Leaf(_), Point(l)) => Some(l)
      case (Node(head, Point(tail)), l) => 
        for {
          ll <- graft(tail, brs)
        } yield Node(head, Point(ll))
    }

  def substitute[A](ttree : Tree[_0, Tree[_0, A]]) : Option[Tree[_0, A]] =
    ttree match {
      case Point(Point(a)) => Some(Point(a))
    }

  def fromXml[A](nd : xml.Node)(implicit xmlRd : XmlReadable[A]) : Tree[_0, A] = 
    nd match {
      case <point>{labelContent}</point> => Point(xmlRd.read(labelContent))
    }

}

abstract class TreeSuccFunctions[N <: Nat] extends TreeFunctions[S[N]] {

  val prev : TreeFunctions[N]

  def dim : S[N] = S(prev.dim)

  // Err.  These should be done as instances I think ...
  def map[A, B](tree : Tree[S[N], A], f : A => B) : Tree[S[N], B] =
    tree match {
      case Leaf(addr) => Leaf(addr)
      case Node(a , shell) => 
        Node(f(a), prev.map(shell, (t : Tree[S[N], A]) => map(t, f)))
    }

  def traverse[G[_], A, B](tree :  Tree[S[N], A], f : A => G[B])(implicit apG : Applicative[G]) : G[Tree[S[N], B]] = {

    import apG._

    tree match {
      case Leaf(addr) => pure(Leaf(addr))
      case Node(a, shell) => {

        val nodeCons : G[(B, Tree[N, Tree[S[N], B]]) => Tree[S[N], B]] = 
          pure((b : B, t : Tree[N, Tree[S[N], B]]) => Node(b, t))

        val traversedShell = 
          prev.traverse(shell, (t : Tree[S[N], A]) => this.traverse(t, f))

        ap2(f(a), traversedShell)(nodeCons)

      }

    }
  }

  def isLeaf[A](tree : Tree[S[N], A]) : Boolean = 
    tree match {
      case Leaf(_) => true
      case _ => false
    }

  def isNode[A](tree : Tree[S[N], A]) : Boolean = 
    tree match {
      case Node(_, _) => true
      case _ => false
    }

  def plug[A](d : Derivative[S[N], A], a : A) : Tree[S[N], A] =
    d match {
      case (shell, context) => close(context, Node(a, shell))
    }

  def close[A](c : Context[S[N], A], t : Tree[S[N], A]) : Tree[S[N], A] =
    c match {
      case Nil => t
      case (a, d) :: cs => close(cs, Node(a, prev.plug(d, t)))
    }

  def emptyContext[A] : Context[S[N], A] = Nil

  def value[A](tree : Tree[S[N], A]) : Option[A] = 
    tree match {
      case Leaf(_) => None
      case Node(a, _) => Some(a)
    }

  def valueAt[A](tree : Tree[S[N], A], addr : Address[S[N]]) : Option[A] = 
    for {
      z <- seek(addr, (tree , Nil))
      r <- value(z._1)
    } yield r

  def zipComplete[A, B](ta : Tree[S[N], A], tb : Tree[S[N], B]) : Option[Tree[S[N], (A, B)]] =
    (ta, tb) match {
      case (Leaf(a0), Leaf(a1)) => Some(Leaf(a0))
      case (Node(a, ash), Node(b, bsh)) => {

        for {
          zippedShells <- prev.zipComplete(ash, bsh)

          resultShell <- prev.traverse(zippedShells, 
            (tpr : (Tree[S[N], A], Tree[S[N], B])) => {
              zipComplete(tpr._1, tpr._2)
            }
          )

        } yield Node((a, b), resultShell)

      }
      case (atr @ Node(a, ash), Leaf(a1)) => {
        println("Matching failed in dimension " ++ toInt(dim).toString ++ " with first tree leaving: " ++ atr.toString)
        None
      }
      case (Leaf(a0), btr @ Node(b, bsh)) => {
        println("Matching failed in dimension " ++ toInt(dim).toString ++ " with second tree leaving: " ++ btr.toString)
        None
      }
    }

  def zipWithPrefix[A](pref : Address[S[N]], tree : Tree[S[N], A]) : Tree[S[N], (A, Address[S[N]])] =
    tree match {
      case Leaf(addr) => Leaf(addr)
      case Node(a, shell) => 
        Node((a, pref), prev.map(prev.zipWithAddress(shell), 
          (apr : (Tree[S[N], A], Address[N])) => {
            zipWithPrefix(apr._2 :: pref, apr._1)
          }))
    }

  def graft[A](tree : Tree[S[S[N]], A], brs : Tree[S[N], Tree[S[S[N]], A]]) : Option[Tree[S[S[N]], A]] = 
    tree match {
      case Leaf(addr) => valueAt(brs, addr)
      case Node(a, shell) => 
        for {
          gsh <- traverse(shell, (t : Tree[S[S[N]], A]) =>
            graft(t, brs)
          )
        } yield Node(a, gsh)

    }

  def substitute[A](ttree : Tree[S[N], Tree[S[N], A]]) : Option[Tree[S[N], A]] = 
    ttree match {
      case Leaf(addr) => Some(Leaf(addr))
      case Node(t, tsh) => 
        for {
          s <- prev.traverse(tsh, 
            (tt : Tree[S[N], Tree[S[N], A]]) => substitute(tt)
          )
          r <- prev.graft(t, s)
        } yield r
    }

  def fromXml[A](nd : xml.Node)(implicit xmlRd : XmlReadable[A]) : Tree[S[N], A] = 
    nd match {
      case <leaf>{addressContent}</leaf> => {
        import XmlReadable.addressReadable
        Leaf(addressReadable(prev.dim).read(addressContent))
      }
      case <node><value>{aContent}</value><shell>{shellContent}</shell></node> => {

        import XmlReadable.treeReadable

        val thisReader = treeReadable(xmlRd, theseFunctions)
        val prevReader = treeReadable(thisReader, prev)

        val a : A = xmlRd.read(aContent)

        val shell : Tree[N, Tree[S[N], A]] = 
          prevReader.read(shellContent)

        Node(a, shell)

      }
    }

}

object TreeOneFunctions extends TreeSuccFunctions[_0] {

  val prev : TreeFunctions[_0] = TreeZeroFunctions

  def visit[A](d : Direction[_1], z : Zipper[_1, A]) : Option[Zipper[_1, A]]  =
    (d, z) match {
      case (Nil, (Node(h, Point(t)), c)) => Some(t, ((h, ()) :: c))
      case (_, _) => None
    }

}

case class TreeDblSuccFunctions[N <: Nat](val pp : TreeFunctions[N]) extends TreeSuccFunctions[S[N]] {

  val prev : TreeFunctions[S[N]] = 
    haveSuccFunctions(pp)

  def visit[A](d : Direction[S[S[N]]], z : Zipper[S[S[N]], A]) : Option[Zipper[S[S[N]], A]] = 
    z match {
      case (Node(a, shell), c) => 
        for {
          z <- prev.seek(d, (shell, Nil))
          br <- (
            z match {
              case (Leaf(addr), zz) => None
              case (Node(t, tsh), zz) => Some((t , (a, (tsh, zz)) :: c ))
            }
          )
        } yield br

      case (_, _) => None
    }

}
