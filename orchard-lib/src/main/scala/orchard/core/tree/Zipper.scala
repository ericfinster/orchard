/**
  * Zipper.scala - Zippers for Higher Dimensional Trees
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import Nats._
import Tree._

//============================================================================================
// Derivatives
//

sealed abstract class Derivative[N <: Nat, +A] {

  def plugWith[B >: A](b : B) : Tree[N, B]

}

case object ZeroDeriv extends Derivative[_0, Nothing] {

  def plugWith[B >: Nothing](b : B) : Tree[_0, B] = Pt(b)

}

case class Open[N <: Nat, +A](sh : Tree[N, Tree[S[N], A]], ct : Context[S[N], A]) extends Derivative[S[N], A] {

  def plugWith[B >: A](b : B) : Tree[S[N], B] = ct closeWith Node(b, sh)

}

//============================================================================================
// Contexts
//

sealed abstract class Context[N <: Nat, +A] {

  def closeWith[B >: A](tr : Tree[N, B]) : Tree[N, B]

}

case class Empty[N <: Nat]() extends Context[N, Nothing] {

  def closeWith[B >: Nothing](tr : Tree[N, B]) : Tree[N, B] = tr

}

case class Then[N <: Nat, +A](a : A, d : Derivative[N, Tree[S[N], A]], ds : Context[S[N], A]) extends Context[S[N], A] {

  def closeWith[B >: A](tr : Tree[S[N], B]) : Tree[S[N], B] = ds closeWith Node(a, d plugWith tr)

}

//============================================================================================
// Zippers
//

sealed abstract class Zipper[N <: Nat, +A] {

  def focus : Tree[N, A]
  def context : Context[N, A]

  def visit(dir : Dir[N]) : Option[Zipper[N, A]]

  def seek(addr : Addr[N]) : Option[Zipper[N, A]] = 
    addr match {
      case Root() => Some(this)
      case Step(d, ds) => 
        for {
          zp <- seek(ds)
          zr <- zp.visit(d)
        } yield zr
    }


}

case class FocusList[+A](lst : Tree[_1, A], ctxt : Context[_1, A]) extends Zipper[_1, A] {

  def focus : Tree[_1, A] = lst
  def context : Context[_1, A] = ctxt

  def visit(dir : Dir[_1]) : Option[Zipper[_1, A]] = 
    lst match {
      case Leaf(_) => None
      case Node(hd, Pt(tl)) => Some(FocusList(tl, Then(hd, ZeroDeriv, ctxt)))
    }

}

case class FocusBranch[N <: Nat, +A](tr : Tree[S[S[N]], A], ctxt : Context[S[S[N]], A]) extends Zipper[S[S[N]], A] {

  def focus : Tree[S[S[N]], A] = tr
  def context : Context[S[S[N]], A] = ctxt

  def visit(dir : Dir[S[S[N]]]) : Option[Zipper[S[S[N]], A]] = 
    tr match {
      case Leaf(_) => None
      case Node(a, sh) => 
        for {
          shZip <- sh seek dir
          resZip <- (
            shZip.focus match {
              case Leaf(_) => None
              case Node(t, tsh) => Some(FocusBranch(t, Then(a, Open(tsh, shZip.context), ctxt)))
            }
          )
        } yield resZip
    }

}


