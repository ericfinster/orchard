/**
  * Pd.scala - Pasting Diagrams
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import scala.language.higherKinds

import Nats._
import Trees._

sealed abstract class Pd[N <: Nat, +A]

case class Obj[+A](a : A) extends Pd[_0, A]
case class Dot[N <: Nat, +A](a : A, cr : Tree[N, Address[N]]) extends Pd[S[N], A]
case class Box[N <: Nat, +A](a : A, sh : Tree[N, Pd[N, A]]) extends Pd[N, A]

object PastingDiagrams {

  type DerivativePd[N <: Nat, +A] = (Tree[N, Pd[N, A]] , ContextPd[N, A])
  type ContextPd[N <: Nat, +A] = List[(A, Derivative[N, Pd[N, A]])]
  type ZipperPd[N <: Nat, +A] = (Pd[N, A], ContextPd[N, A])

  implicit def haveZeroPdFunctions : PdFunctions[_0] = PdZeroFunctions

  implicit def haveSuccPdFunctions[N <: Nat](implicit prev : PdFunctions[N]) : PdFunctions[S[N]] = 
    new PdSuccFunctions[N] {

      val prevTfns : TreeFunctions[N] = prev.tfns
      implicit val tfns : TreeFunctions[S[N]] = haveSuccFunctions(prev.tfns)

    }

}

