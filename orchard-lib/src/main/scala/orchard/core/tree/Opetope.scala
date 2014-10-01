/**
  * Opetope.scala - Opetopes
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import scala.language.higherKinds
import scala.language.implicitConversions

import scalaz.{Tree => _, _}
import scalaz.Id._

import Nats._
import Trees._

// sealed trait TargetComplex[N <: Nat, +A]
// case class Base[+A](a : A) extends TargetComplex[_0, A]
// case class Extend[N <: Nat, +A](prefix : TargetComplex[N, A], tree : Tree[S[N], (A, A)]) extends TargetComplex[S[N], A]

// sealed trait OpetopicComplex[N <: Nat, +A]
// case class Objct[+A](a : A) extends OpetopicComplex[_0, A]
// case class Comp[N <: Nat, +A](cmplx : TargetComplex[N, A], filler : A, target : A) extends OpetopicComplex[S[N], A]

object Opetopes {

  type OpetopicComplex[N <: Nat, +A] = N#OpetopicComplex[A]

  import TreeExamples._

  val fredOpetope : OpetopicComplex[_4, Int] = ((((fredT3, fredT2), fredT1), fred), (27, 26))

  trait OpetopicFunctions[N <: Nat] {

  }

  object OpetopeZeroFunctions extends OpetopicFunctions[_0] {

  }

  trait OpetopicSuccFunctions[N <: Nat] extends OpetopicFunctions[S[N]] {

    val prev : OpetopicFunctions[N]

  }

  // val test2 = Base :+ 0 :+ ex0 :> (3, 4)

  // implicit class ComplexOps[N <: Nat, +A](cmplx : Complex[N, A]) {

  //   def :+[B >: A](tr : Tree[N, B]) : Complex[S[N], B] = 
  //     Extend[N, B](cmplx, tr)

  //   def :>[B >: A](pr : (B, B)) : Opetope[N, B] = 
  //     Opetope(cmplx, pr._1, pr._2)

  // }

}

