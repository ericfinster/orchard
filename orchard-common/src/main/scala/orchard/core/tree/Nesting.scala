/**
  * Nesting.scala - Nestings
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import scalaz.{Tree => _, Zipper => _, _}
import scalaz.std.option._

import Dir._
import Nats._
import Tree._

sealed abstract class Nesting[N <: Nat, +A] {

  def dim : N

  def label : A
  def spine : Option[Tree[N, A]]

}

case class Obj[+A](a : A) extends Nesting[_0, A] {

  def dim = Z

  def label : A = a
  def spine : Option[Tree[_0, A]] = Some(Pt(a))

}

case class Dot[N <: Nat, +A](a : A, c : Tree[N, Addr[N]]) extends Nesting[S[N], A] {

  def dim = S(c.dim)

  def label : A = a
  def spine : Option[Tree[S[N], A]] = Some(Node(a, map(c)(Leaf(_))))

}

case class Box[N <: Nat, +A](a : A, c : Tree[N, Nesting[N, A]]) extends Nesting[N, A] {

  def dim = c.dim

  def label : A = a
  def spine : Option[Tree[N, A]] = 
    for {
      st <- traverse(c)(_.spine)
      sp <- join(st)
    } yield sp

}

object NestingExamples {

  val fred0 : Nesting[_0, Int] = Box(4, Pt(Box(3, Pt(Box(2, Pt(Obj(1)))))))

  val fred1 : Nesting[_1, Int] = Box(13, Node(Box(12, Node(Dot(7, Pt(Root())),Pt(Leaf(Root())))),Pt(Node(Box(11, Node(Box(10, Leaf(Root())),Pt(Node(Dot(6, Pt(Root())),Pt(Leaf(Root())))))),Pt(Node(Box(9, Node(Box(8, Node(Dot(5, Pt(Root())),Pt(Leaf(Root())))),Pt(Leaf(Root())))),Pt(Leaf(Root()))))))))

  val fred2 : Nesting[_2, Int] = Box(22, Node(Box(21, Node(Dot(18, Node(Root(),Pt(Node(Step(Root(), Root()),Pt(Node(Step(Root(), Step(Root(), Root())),Pt(Leaf(Root())))))))),Node(Leaf(Root()),Pt(Node(Node(Dot(17, Node(Root(),Pt(Node(Step(Root(), Root()),Pt(Leaf(Root())))))),Node(Node(Dot(16, Leaf(Root())),Leaf(Root())),Pt(Node(Leaf(Step(Root(), Root())),Pt(Leaf(Root())))))),Pt(Node(Leaf(Step(Root(), Step(Root(), Root()))),Pt(Leaf(Root()))))))))),Node(Node(Dot(19, Node(Root(),Pt(Leaf(Root())))),Node(Leaf(Root()),Pt(Leaf(Root())))),Pt(Node(Leaf(Step(Root(), Root())),Pt(Node(Node(Box(20, Node(Dot(15, Node(Root(),Pt(Leaf(Root())))),Node(Leaf(Root()),Pt(Leaf(Root()))))),Node(Node(Dot(14, Node(Root(),Pt(Leaf(Root())))),Node(Leaf(Step(Root(), Step(Root(), Root()))),Pt(Leaf(Root())))),Pt(Leaf(Root())))),Pt(Leaf(Root())))))))))

  val fred3 : Nesting[_3, Int] = Box(26, Node(Dot(25, Node(Root(),Node(Node(Step(Root(), Root()),Node(Leaf(Root()),Pt(Leaf(Root())))),Pt(Node(Leaf(Step(Root(), Root())),Pt(Node(Node(Step(Step(Root(), Step(Root(), Root())), Root()),Node(Node(Step(Root(), Step(Step(Root(), Step(Root(), Root())), Root())),Node(Leaf(Step(Root(), Step(Root(), Root()))),Pt(Leaf(Root())))),Pt(Leaf(Root())))),Pt(Leaf(Root()))))))))),Node(Node(Dot(24, Node(Root(),Node(Leaf(Root()),Pt(Node(Node(Step(Step(Root(), Root()), Root()),Node(Node(Step(Root(), Step(Step(Root(), Root()), Root())),Leaf(Root())),Pt(Node(Leaf(Step(Root(), Root())),Pt(Leaf(Root())))))),Pt(Node(Leaf(Step(Root(), Step(Root(), Root()))),Pt(Leaf(Root()))))))))),Node(Leaf(Root()),Node(Leaf(Root()),Pt(Node(Node(Leaf(Step(Step(Root(), Root()), Root())),Node(Node(Leaf(Step(Root(), Step(Step(Root(), Root()), Root()))),Leaf(Root())),Pt(Node(Leaf(Step(Root(), Root())),Pt(Leaf(Root())))))),Pt(Node(Leaf(Step(Root(), Step(Root(), Root()))),Pt(Leaf(Root()))))))))),Node(Node(Leaf(Step(Root(), Root())),Node(Leaf(Root()),Pt(Leaf(Root())))),Pt(Node(Leaf(Step(Root(), Root())),Pt(Node(Node(Node(Dot(23, Node(Root(),Node(Leaf(Root()),Pt(Leaf(Root()))))),Node(Leaf(Step(Step(Root(), Step(Root(), Root())), Root())),Node(Leaf(Root()),Pt(Leaf(Root()))))),Node(Node(Leaf(Step(Root(), Step(Step(Root(), Step(Root(), Root())), Root()))),Node(Leaf(Step(Root(), Step(Root(), Root()))),Pt(Leaf(Root())))),Pt(Leaf(Root())))),Pt(Leaf(Root()))))))))))

  val fred4 : Nesting[_4, Int] = Dot(27, Node(Root(),Node(Node(Step(Root(), Root()),Node(Leaf(Root()),Node(Leaf(Root()),Pt(Node(Node(Leaf(Step(Step(Root(), Root()), Root())),Node(Node(Leaf(Step(Root(), Step(Step(Root(), Root()), Root()))),Leaf(Root())),Pt(Node(Leaf(Step(Root(), Root())),Pt(Leaf(Root())))))),Pt(Node(Leaf(Step(Root(), Step(Root(), Root()))),Pt(Leaf(Root()))))))))),Node(Node(Leaf(Step(Root(), Root())),Node(Leaf(Root()),Pt(Leaf(Root())))),Pt(Node(Leaf(Step(Root(), Root())),Pt(Node(Node(Node(Step(Step(Step(Root(), Step(Root(), Root())), Root()), Root()),Node(Leaf(Step(Step(Root(), Step(Root(), Root())), Root())),Node(Leaf(Root()),Pt(Leaf(Root()))))),Node(Node(Leaf(Step(Root(), Step(Step(Root(), Step(Root(), Root())), Root()))),Node(Leaf(Step(Root(), Step(Root(), Root()))),Pt(Leaf(Root())))),Pt(Leaf(Root())))),Pt(Leaf(Root()))))))))))

  val fred : Complex[_4, Int] = Append(Append(Append(Append(Base(fred0), fred1), fred2), fred3), fred4)

}
