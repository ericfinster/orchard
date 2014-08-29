/**
  * Slice.scala - Trying to slice
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import scala.language.higherKinds
import scala.language.implicitConversions

object Unindexed {

  //============================================================================================
  // THE SLICE
  //

  sealed abstract class Slice[M[_], A] 

  case class Cap[M[_], A]() extends Slice[M, A]
  case class Joint[M[_], A](n : A, sma : M[Slice[M, A]]) extends Slice[M, A]

  //============================================================================================
  // EXAMPLES
  //

  type Term[A] = Unit

  type SliceTerm[A] = Slice[Term, A]

  val si0 : SliceTerm[String] = Cap[Term, String]()
  val si1 : SliceTerm[String] = Joint[Term, String]("x", ())

  object SNull { def apply[A]() : SliceTerm[A] = Cap[Term, A]() }
  object SId { def apply[A](n : A) : SliceTerm[A] = Joint[Term, A](n, ()) }

  type SliceList[A] = Slice[SliceTerm, A]

  def nil[A]() : SliceList[A] = Cap()
  def cons[A](n : A, l : SliceList[A]) : SliceList[A] = Joint(n, SId(l))

  val f : SliceList[String] = cons("f", nil())
  val g : SliceList[String] = cons("g", nil())
  // ...

  val as : SliceList[String] = nil()
  val bs : SliceList[String] = cons("f", cons("i", nil()))
  val cs : SliceList[String] = cons("g", nil())
  val ds : SliceList[String] = cons("k", cons("j", cons("h", nil())))

  val es : SliceList[String] = cons("f", cons("g", cons("h", nil())))

  type SliceTree[A] = Slice[SliceList, A]

  def leaf[A]() : SliceTree[A] = Cap()
  def node[A](a : A, bs : SliceList[SliceTree[A]]) : SliceTree[A] = Joint(a, bs)

  val a : SliceTree[String] = node("a", nil())
  val b : SliceTree[String] = node("b", cons(leaf(), cons(a, nil())))
  val c : SliceTree[String] = node("c", cons(leaf(), nil()))
  val d : SliceTree[String] = node("d", cons(b, cons(c, nil())))

  val e : SliceTree[String] = node("e", cons(leaf(), cons(leaf(), cons(leaf(), nil()))))

  //============================================================================================
  // SLICE FIX
  //

  // Right, how to fix these ....

  abstract class SliceFix[S[_[_], _], M[_], A]
  case class Cup[S[_[_], _], M[_], A](c : M[A]) extends SliceFix[S, M, A]
  case class Fix[S[_[_], _], M[_], A](x : S[({ type L[X] = SliceFix[S, M, X]})#L, A]) extends SliceFix[S, M, A]

  type SliceTermFix[A] = SliceFix[Slice, Term, A]

  type Slice0[A] = Term[A]
  type Slice1[A] = Slice[Slice0, A]
  type Slice2[A] = Slice[Slice1, A]
  type Slice3[A] = Slice[Slice2, A]
  // ...

  val sf0 : SliceTermFix[String] = Cup[Slice, Term, String](())

  type FixType[A] = Slice[SliceTermFix, A]


  val ft0 : FixType[String] = Cap[SliceTermFix, String]()

  type GoalType = SliceTermFix[Slice[SliceTermFix, String]]
  val goal : GoalType = ???

  val ft1 : FixType[String] = Joint[SliceTermFix, String]("x", goal)

  val sf1 : SliceTermFix[String] = Fix[Slice, Term, String](ft1)

  // case class Cap[M[_], A]() extends Slice[M, A]
  // case class Joint[M[_], A](n : A, sma : M[Slice[M, A]]) extends Slice[M, A]

  //============================================================================================
  // IDEA
  //

  // Right, here is the last idea before you move on and just give up.  The idea is that you
  // "factor" the joint construction into a couple different types of cells: either a cell is
  // negative (external), neutral (internal), or positive (a target).  The only place where
  // these kind of coincide is the top cell.

  // So the hope is that by distinguishing among them, you will have places to put the labels.

  // The second half of this idea (and in fact, the one I think you should try first) is to
  // actually fix the slice construction itself, in order to have a single type with all
  // of these guys included.  The constructors of that guys might give a hint ....

  // Yes, I simply don't see a way to get what you want by simply working with the pasting diagrams.
  // There is some kind of interlocking interaction between changing the dimension by closing a cell
  // and where to label the data.

  // Right.  We are not distinguishing between closed cells and pasting diagrams with only a single
  // "depth" level.  And this is super fishy.  We need to use this second fixedpoint guy to do
  // that.  Although I don't quite see how yet ....

}
