/**
  * Tr.scala - Testing with indexed definitions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import scala.language.higherKinds
import scala.language.implicitConversions
import scalaz.{Tree => _, Zipper => _, _}
import Nats._

sealed abstract class Dir[N <: Nat]
case class Root[N <: Nat]() extends Dir[S[N]]
case class Step[N <: Nat](d : Dir[N], ds : Dir[S[N]]) extends Dir[S[N]]


sealed abstract class Tree[N <: Nat, +A] {

  def map[B](f : A => B) : Tree[N, B]
  def traverse[G[_], B](f : A => G[B])(implicit apG : Applicative[G]) : G[Tree[N, B]] 

}

case class Pt[+A](a : A) extends Tree[_0, A] {

  def map[B](f : A => B) : Tree[_0, B] = Pt(f(a))

  def traverse[G[_], B](f : A => G[B])(implicit apG : Applicative[G]) : G[Tree[_0, B]] = { 
    import apG._ 
    ap(f(a))(pure((b : B) => Pt(b))) 
  }

}

case class Leaf[N <: Nat](addr : Dir[S[N]]) extends Tree[S[N], Nothing] {

  def map[B](f : Nothing => B) : Tree[S[N], Nothing] = this

  def traverse[G[_], B](f : Nothing => G[B])(implicit apG : Applicative[G]) : G[Tree[S[N], B]] = {
    import apG._
    pure(this)
  }

}

case class Node[N <: Nat, +A](a : A, shell : Tree[N, Tree[S[N], A]]) extends Tree[S[N], A] {

  def map[B](f : A => B) : Tree[S[N], B] = Node(f(a), shell map (_.map(f)))

  def traverse[G[_], B](f : A => G[B])(implicit apG : Applicative[G]) : G[Tree[S[N], B]] = {
    import apG._

    ???
  }

}

object Tree {

  type Addr[N <: Nat] = Dir[S[N]]

  implicit def asRootZipper[N <: Nat, A](tr : Tree[N, A]) : Zipper[N, A] = ???

}



// sealed trait TreeFunctions[N <: Nat] {

//   val dim : N

//   def succ : TreeFunctions[S[N]]

//   def map[A, B](tree : Tree[N, A], f : A => B) : Tree[N, B]
//   def traverse[G[_], A, B](tree :  Tree[N, A], f : A => G[B])(implicit apG : Applicative[G]) : G[Tree[N, B]]

//   def isLeaf[A](tree : Tree[N, A]) : Boolean
//   def isNode[A](tree : Tree[N, A]) : Boolean

//   def plug[A](d : Derivative[N, A], a : A) : Tree[N, A] 
//   def close[A](c : Context[N, A], t : Tree[N, A]) : Tree[N, A]

//   def emptyContext[A] : Context[N, A]

//   def visit[A](d : Direction[N], z : Zipper[N, A]) : Option[Zipper[N, A]] 
//   def seek[A](a : Address[N], z : Zipper[N, A]) : Option[Zipper[N, A]] =
//     a match {
//       case Nil => Some(z)
//       case d :: ds => 
//         for {
//           zz <- seek(ds, z)
//           zzz <- visit(d, zz)
//         } yield zzz
//     }

//   def value[A](tree : Tree[N, A]) : Option[A]
//   def valueAt[A](tree : Tree[N, A], addr : Address[N]) : Option[A]

//   def zipComplete[A, B](ta : Tree[N, A], tb : Tree[N, B]) : Option[Tree[N, (A, B)]]
//   def zipWithPrefix[A](pref : Address[N], tree : Tree[N, A]) : Tree[N, (A, Address[N])]
//   def zipWithAddress[A](tree : Tree[N, A]) : Tree[N, (A, Address[N])] =
//     zipWithPrefix(Nil, tree)

//   def graft[A](tree : Tree[S[N], A], brs : Tree[N, Tree[S[N], A]]) : Option[Tree[S[N], A]] 
//   def substitute[A](ttree : Tree[N, Tree[N, A]]) : Option[Tree[N, A]]

//   def fromXml[A](nd : xml.Node)(implicit xmlRd : XmlReadable[A]) : Tree[N, A]

// }

// case object TreeZeroFunctions extends TreeFunctions[_0] {

//   val dim : _0 = Z

//   def succ : TreeFunctions[_1] = TreeOneFunctions

//   def map[A, B](tree : Tree[_0, A], f : A => B) : Tree[_0, B] = 
//     tree match {
//       case Point(a) => Point(f(a))
//     }

//   def traverse[G[_], A, B](tree :  Tree[_0, A], f : A => G[B])(implicit apG : Applicative[G]) : G[Tree[_0, B]] = 
//     tree match {
//       case Point(a) => { import apG._ ; ap(f(a))(pure((b : B) => Point(b))) }
//     }

//   def isLeaf[A](tree : Tree[_0, A]) : Boolean = false
//   def isNode[A](tree : Tree[_0, A]) : Boolean = true

//   def plug[A](d : Derivative[_0, A], a : A) : Tree[_0, A] = Point(a)
//   def close[A](c : Context[_0, A], t : Tree[_0, A]) : Tree[_0, A] = t

//   def emptyContext[A] : Context[_0, A] = ()

//   def visit[A](d : Direction[_0], z : Zipper[_0, A]) : Option[Zipper[_0, A]] = None

//   def value[A](tree : Tree[_0, A]) : Option[A] = 
//     tree match {
//       case Point(a) => Some(a)
//     }

//   def valueAt[A](tree : Tree[_0, A], addr : Address[_0]) : Option[A] = 
//     value(tree)

//   def zipComplete[A, B](ta : Tree[_0, A], tb : Tree[_0, B]) : Option[Tree[_0, (A, B)]] =
//     (ta, tb) match {
//       case (Point(a), Point(b)) => Some(Point((a, b)))
//     }

//   def zipWithPrefix[A](pref : Address[_0], tree : Tree[_0, A]) : Tree[_0, (A, Address[_0])] = 
//     tree match {
//       case Point(a) => Point(a, Nil)
//     }


//   def graft[A](tree : Tree[_1, A], brs : Tree[_0, Tree[_1, A]]) : Option[Tree[_1, A]] = 
//     (tree, brs) match {
//       case (Leaf(_), Point(l)) => Some(l)
//       case (Node(head, Point(tail)), l) => 
//         for {
//           ll <- graft(tail, brs)
//         } yield Node(head, Point(ll))
//     }

//   def substitute[A](ttree : Tree[_0, Tree[_0, A]]) : Option[Tree[_0, A]] =
//     ttree match {
//       case Point(Point(a)) => Some(Point(a))
//     }

//   def fromXml[A](nd : xml.Node)(implicit xmlRd : XmlReadable[A]) : Tree[_0, A] = 
//     nd match {
//       case <point>{labelContent}</point> => Point(xmlRd.read(labelContent))
//     }

// }

// trait TreeSuccFunctions[N <: Nat] extends TreeFunctions[S[N]] {

//   def prev : TreeFunctions[N]

//   def map[A, B](tree : Tree[S[N], A], f : A => B) : Tree[S[N], B] = ???
//     // tree.map(f)(treeIsTraverse[N](prev))

//   def traverse[G[_], A, B](tree :  Tree[S[N], A], f : A => G[B])(implicit apG : Applicative[G]) : G[Tree[S[N], B]] = ???
//     // tree.traverse(f)(apG, treeIsTraverse[N](prev))

//   def isLeaf[A](tree : Tree[S[N], A]) : Boolean = 
//     tree match {
//       case Leaf(_) => true
//       case _ => false
//     }

//   def isNode[A](tree : Tree[S[N], A]) : Boolean = 
//     tree match {
//       case Node(_, _) => true
//       case _ => false
//     }

//   def plug[A](d : Derivative[S[N], A], a : A) : Tree[S[N], A] =
//     d match {
//       case (shell, context) => close(context, Node(a, shell))
//     }

//   def close[A](c : Context[S[N], A], t : Tree[S[N], A]) : Tree[S[N], A] =
//     c match {
//       case Nil => t
//       case (a, d) :: cs => close(cs, Node(a, prev.plug(d, t)))
//     }

//   def emptyContext[A] : Context[S[N], A] = Nil

//   def value[A](tree : Tree[S[N], A]) : Option[A] = 
//     tree match {
//       case Leaf(_) => None
//       case Node(a, _) => Some(a)
//     }

//   def valueAt[A](tree : Tree[S[N], A], addr : Address[S[N]]) : Option[A] = 
//     for {
//       z <- seek(addr, (tree , Nil))
//       r <- value(z._1)
//     } yield r

//   def zipComplete[A, B](ta : Tree[S[N], A], tb : Tree[S[N], B]) : Option[Tree[S[N], (A, B)]] =
//     (ta, tb) match {
//       case (Leaf(a0), Leaf(a1)) => Some(Leaf(a0))
//       case (Node(a, ash), Node(b, bsh)) => {

//         for {
//           zippedShells <- prev.zipComplete(ash, bsh)

//           resultShell <- prev.traverse(zippedShells, 
//             (tpr : (Tree[S[N], A], Tree[S[N], B])) => {
//               zipComplete(tpr._1, tpr._2)
//             }
//           )

//         } yield Node((a, b), resultShell)

//       }
//       case (atr @ Node(a, ash), Leaf(a1)) => {
//         // println("Matching failed in dimension " ++ toInt(dim).toString ++ " with first tree leaving: " ++ atr.toString)
//         None
//       }
//       case (Leaf(a0), btr @ Node(b, bsh)) => {
//         // println("Matching failed in dimension " ++ toInt(dim).toString ++ " with second tree leaving: " ++ btr.toString)
//         None
//       }
//     }

//   def zipWithPrefix[A](pref : Address[S[N]], tree : Tree[S[N], A]) : Tree[S[N], (A, Address[S[N]])] =
//     tree match {
//       case Leaf(addr) => Leaf(addr)
//       case Node(a, shell) => 
//         Node((a, pref), prev.map(prev.zipWithAddress(shell), 
//           (apr : (Tree[S[N], A], Address[N])) => {
//             zipWithPrefix(apr._2 :: pref, apr._1)
//           }))
//     }

//   def graft[A](tree : Tree[S[S[N]], A], brs : Tree[S[N], Tree[S[S[N]], A]]) : Option[Tree[S[S[N]], A]] = 
//     tree match {
//       case Leaf(addr) => valueAt(brs, addr)
//       case Node(a, shell) => 
//         for {
//           gsh <- traverse(shell, (t : Tree[S[S[N]], A]) =>
//             graft(t, brs)
//           )
//         } yield Node(a, gsh)

//     }

//   def substitute[A](ttree : Tree[S[N], Tree[S[N], A]]) : Option[Tree[S[N], A]] = 
//     ttree match {
//       case Leaf(addr) => Some(Leaf(addr))
//       case Node(t, tsh) => 
//         for {
//           s <- prev.traverse(tsh, 
//             (tt : Tree[S[N], Tree[S[N], A]]) => substitute(tt)
//           )
//           r <- prev.graft(t, s)
//         } yield r
//     }

//   def fromXml[A](nd : xml.Node)(implicit xmlRd : XmlReadable[A]) : Tree[S[N], A] = 
//     nd match {
//       case <leaf>{addressContent}</leaf> => {
//         import XmlReadable.addressReadable
//         //Leaf(addressReadable(prev.dim).read(addressContent))
//         ???
//       }
//       case <node><value>{aContent}</value><shell>{shellContent}</shell></node> => {

//         import XmlReadable.treeReadable

//         val thisReader = treeReadable(xmlRd, this)
//         val prevReader = treeReadable(thisReader, prev)

//         val a : A = xmlRd.read(aContent)

//         val shell : Tree[N, Tree[S[N], A]] = 
//           prevReader.read(shellContent)

//         Node(a, shell)

//       }
//     }

// }


