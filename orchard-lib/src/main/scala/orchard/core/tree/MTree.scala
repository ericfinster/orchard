/**
  * MTree.scala - Playing with higher dimensional trees
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import scala.language.higherKinds

import orchard.core.util._
import Nats._

sealed trait MTree[A, D <: Nat]
case class Obj[A]() extends MTree[A, _0]
case class Drop[A, D <: Nat](shape : MTree[Unit, D]) extends MTree[A, S[S[D]]]
case class Node[A, D <: Nat](a : A, mtree : MTree[MTree[A, S[D]], D]) extends MTree[A, S[D]]

object MTree {

  def empty[A] : MTree[A, _0] = Obj()

  def test[A] : MTree[A, _2] = Drop(Obj())

  def arrow[A](a : A) : MTree[A, _1] = Node(a, empty)

}

object Ghani {

  sealed trait RoseFix[F[+_], +A]
  case class Rose[F[+_], +A](a : A, rec : Option[F[RoseFix[F, A]]]) extends RoseFix[F, A]

  case class Rose0[+A](a : A)

  type Rose1[+A] = RoseFix[Rose0, A]
  type Rose2[+A] = RoseFix[Rose1, A]
  type Rose3[+A] = RoseFix[Rose2, A]

  // Right, and the point is the use of option here.  Instead of option, we want to be able to
  // sick in and previous dimensional guy.

  // What I'd like to say is that what we are encoding here is the *list of targets* of a pasting
  // diagram.  What's missing is the initial object.  Or, it's the list of arrows, although nothing
  // is labelling the objects here ....

  // Right.  The data types presented here label the cells of the dimension specified, but do not
  // in any way label the lower dimensions.  And the reason we have nothing to label the empty
  // list with is that, well, I dunno.

  // Ah, right, we are listing the *source cells* in a pasting diagram.  In the case of a 2-drop,
  // there is no arrow source.

  type ConsOf[F[+_]] = F[Unit]

  sealed trait Slice[F[+_], +A]
  case class SliceUnit[F[+_], +A](fa : F[A]) extends Slice[F, A]
  case class SliceFix[F[+_], +A](a : A, rec : F[Slice[F, A]]) extends Slice[F, A]

  case class Slice0[+A](a : A)

  type Slice1[+A] = Slice[Slice0, A]
  type Slice2[+A] = Slice[Slice1, A]
  type Slice3[+A] = Slice[Slice2, A]

  // def example : Slice1[String] = 
  //   SliceFix("i", Slice0(SliceFix("h", Slice0(SliceUnit(Slice0(()))))))

  // def encodeList[A](l : List[A]) : Slice1[A] = 
  //   l match {
  //     case Nil => SliceUnit(Slice0(()))
  //     case x :: xs => SliceFix(x, Slice0(encodeList(xs)))
  //   }

  // def encodeNat(n : Int) : Slice1[Unit] = 
  //   if (n <= 0) {
  //     SliceUnit(Slice0(()))
  //   } else {
  //     SliceFix((), Slice0(encodeNat(n - 1)))
  //   }

  // def nCorolla[A](n : Int) : Slice2[A] =
  //   SliceUnit[Slice1, A](encodeNat(n))

  def a : Slice2[String] = 
    SliceFix("a", ???)

  // def example : Slice1[String] = 
  //   SliceFix("i", Slice0(("w",
  //     SliceFix("h", Slice0(("z", 
  //       SliceFix("g", Slice0(("y",
  //         SliceFix("f", Slice0(("x", SliceUnit())))
  //       )))
  //     )))
  //   )))


  // def aTop : Slice1[(String, Slice2[String])] =
  //   SliceFix(("g", SliceUnit()), Slice0((("y", SliceUnit()),
  //     SliceFix(("f", SliceUnit()), Slice0((("x", SliceUnit()), SliceUnit())))
  //   )))

  // def aTop : Slice1[Slice2[String]] = 
  //   SliceFix(SliceUnit("g"), Slice0(SliceFix(SliceUnit(???), Slice0(???))))

  // def a : Slice2[String] = 
  //   SliceFix("a",
  //     aTop
  //   )

  // def bTop : Slice1[(String, Slice2[String])] = 
  //   SliceFix(("h", SliceUnit()), Slice0((("z", SliceUnit()), SliceUnit())))

  // def b : Slice2[String] = 
  //   SliceFix("b", bTop)

  // def dTop : Slice1[(String, Slice2[String])] = 
  //   SliceUnit()

  // def d : Slice2[String] = 
  //   SliceFix("d", dTop)

  // def mTop : Slice1[(String, Slice2[String])] = 
  //   SliceFix(("j", a), Slice0(("x", ???), SliceFix(("k", b), ???)))

  // Problems, problems.

  // The thing is that what should happen somehow is that you delay mentioning the sources
  // until you get out to the end.  Right.  It's like the unit guy should store this information.

}
