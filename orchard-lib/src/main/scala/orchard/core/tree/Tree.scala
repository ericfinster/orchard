/**
  * Tr.scala - Testing with indexed definitions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import scala.language.implicitConversions
import scalaz.{Tree => _, Zipper => _, _}
import Nats._

sealed abstract class Dir[N <: Nat]
case class Root[N <: Nat]() extends Dir[S[N]]
case class Step[N <: Nat](d : Dir[N], ds : Dir[S[N]]) extends Dir[S[N]]


sealed abstract class Tree[N <: Nat, +A] {

  def map[B](f : A => B) : Tree[N, B]

}

case class Pt[+A](a : A) extends Tree[_0, A] {

  def map[B](f : A => B) : Tree[_0, B] = Pt(f(a))

}

case class Leaf[N <: Nat](addr : Dir[S[N]]) extends Tree[S[N], Nothing] {

  def map[B](f : Nothing => B) : Tree[S[N], Nothing] = this

}

case class Node[N <: Nat, +A](a : A, shell : Tree[N, Tree[S[N], A]]) extends Tree[S[N], A] {

  def map[B](f : A => B) : Tree[S[N], B] = Node(f(a), shell map (_.map(f)))

}

object Tree {

  type Addr[N <: Nat] = Dir[S[N]]

  implicit def asRootZipper[N <: Nat, A](tr : Tree[N, A]) : Zipper[N, A] = ???

}


