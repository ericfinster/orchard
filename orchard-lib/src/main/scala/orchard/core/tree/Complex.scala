/**
  * Complex.scala - Complexes of pasting diagrams
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import scala.language.higherKinds

import Nats._
import Trees._
import PastingDiagrams._

sealed trait Complex[N <: Nat, +A]

case class Base[+A](pd : Pd[_0, A]) extends Complex[_0, A]
case class Extend[N <: Nat, +A](cm : Complex[N, A], pd : Pd[S[N], A]) extends Complex[S[N], A]


