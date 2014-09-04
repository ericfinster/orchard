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

sealed trait Complex[N <: Nat, +A]
case object Base extends Complex[__0, Nothing]
case class Extend[N <: Nat, +A](prefix : Complex[N, A], tr : Tree[N, A]) extends Complex[S[N], A]


// Wrong, because the complex should take pairs in higher dimensions ...
case class Opetope[N <: Nat, A](cmplx : Complex[N, A], filler : A, target : A)

object Opetopes {

  // val test2 = Base :+ 0 :+ ex0 :> (3, 4)

  implicit class ComplexOps[N <: Nat, +A](cmplx : Complex[N, A]) {

    def :+[B >: A](tr : Tree[N, B]) : Complex[S[N], B] = 
      Extend[N, B](cmplx, tr)

    def :>[B >: A](pr : (B, B)) : Opetope[N, B] = 
      Opetope(cmplx, pr._1, pr._2)

  }

}

