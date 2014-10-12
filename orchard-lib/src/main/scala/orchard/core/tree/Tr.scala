/**
  * Tr.scala - Testing with indexed definitions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import Nats._

sealed abstract class Dir[N <: Nat]
case class Nl[N <: Nat]() extends Dir[S[N]]
case class Cn[N <: Nat](d : Dir[N], ds : Dir[S[N]]) extends Dir[S[N]]

sealed abstract class Tr[N <: Nat, +A]
case class Pt[+A](a : A) extends Tr[_0, A]
case class Lf[N <: Nat](addr : Dir[S[N]]) extends Tr[S[N], Nothing]
case class Nd[N <: Nat, +A](a : A, shell : Tr[N, Tr[S[N], A]]) extends Tr[S[N], A]

//============================================================================================
// Contexts
//

sealed abstract class Cntxt[N <: Nat, +A] {

  def closeWith[B >: A](tr : Tr[N, B]) : Tr[N, B]

}

case class Empty[N <: Nat]() extends Cntxt[N, Nothing] {

  def closeWith[B >: Nothing](tr : Tr[N, B]) : Tr[N, B] = tr

}

case class Then[N <: Nat, +A](a : A, d : Deriv[N, Tr[S[N], A]], ds : Cntxt[S[N], A]) extends Cntxt[S[N], A] {

  def closeWith[B >: A](tr : Tr[S[N], B]) : Tr[S[N], B] = ds closeWith Nd(a, d plugWith tr)

}

//============================================================================================
// Derivatives
//

sealed abstract class Deriv[N <: Nat, +A] {

  def plugWith[B >: A](b : B) : Tr[N, B]

}

case class Sh[N <: Nat, +A](sh : Tr[N, Tr[S[N], A]], ct : Cntxt[S[N], A]) extends Deriv[S[N], A] {

  def plugWith[B >: A](b : B) : Tr[S[N], B] = ct closeWith Nd(b, sh)

}

//============================================================================================
// Zippers
//

sealed abstract class Zip[N <: Nat, +A] {

  def visit(dir : Dir[N]) : Option[Zip[N, A]]

}

case class Fcs[N <: Nat, +A](tr : Tr[S[N], A], cn : Cntxt[S[N], A]) extends Zip[S[N], A] {

  def visit(dir : Dir[S[N]]) : Option[Zip[S[N], A]] = 
    tr match {
      case Lf(_) => None
      case Nd(_, sh) => ???
    }

}

object Tests {

  type Address[N <: Nat] = Dir[S[N]]

  val dir1 : Dir[_1] = Nl()
  val dir2 : Dir[_2] = Nl()

  val addr1 : Address[_1] = Cn(Nl(), Cn(Nl(), Nl()))

  val lst : Tr[_1, Int] = Nd(2, Pt(Nd(1, Pt(Lf(Nl())))))
  val tr : Tr[_2, Int] = 
    Nd(5, 
      Nd(Lf(Nl()), 
      Pt(Nd(Lf(Nl()),
      Pt(Nd(Lf(Nl()), 
      Pt(Lf(Nl()))))))))

}

