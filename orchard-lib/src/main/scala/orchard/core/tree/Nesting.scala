/**
  * Nesting.scala - Nestings
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import scalaz.{Tree => _, Zipper => _, _}
import scalaz.std.option._

import Nats._
import Tree._

sealed abstract class Nesting[N <: Nat, +A] {

  def spine : Option[Tree[N, A]]

}

case class Obj[+A](a : A) extends Nesting[_0, A] {

  def spine : Option[Tree[_0, A]] = Some(Pt(a))

}

case class Dot[N <: Nat, +A](a : A, c : Tree[N, Addr[N]]) extends Nesting[S[N], A] {

  def spine : Option[Tree[S[N], A]] = Some(Node(a, c map (Leaf(_)(c.dim))))

}

case class Box[N <: Nat, +A](a : A, c : Tree[N, Nesting[N, A]]) extends Nesting[N, A] {

  def spine : Option[Tree[N, A]] = 
    for {
      st <- c traverse (_.spine)
      sp <- join(st)
    } yield sp

}

