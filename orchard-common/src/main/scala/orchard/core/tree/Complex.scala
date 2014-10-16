/**
  * Complex.scala - Complexes
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import Nats._
import Tree._

sealed abstract class Complex[N <: Nat, +A] {

  def dim : N
  def head : Nesting[N, A]

}

case class Base[+A](objs : Nesting[_0, A]) extends Complex[_0, A] {

  def dim = Z
  def head : Nesting[_0, A] = objs

}

case class Append[N <: Nat, +A](tl : Complex[N, A], hd : Nesting[S[N], A]) extends Complex[S[N], A] {

  def dim = hd.dim
  def head : Nesting[S[N], A] = hd

}

