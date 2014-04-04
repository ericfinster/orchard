/**
  * Expression.scala - Simple opetopic expressions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

import scala.collection.mutable.Map

import Environment._

trait HasEmpty[A] {

  def empty : A

  def isEmpty(a : A) : Boolean = 
    a == empty

}

object HasEmpty {

  implicit def seqHasEmpty[A] : HasEmpty[Seq[A]] = 
    new HasEmpty[Seq[A]] {
      def empty = Seq.empty
      override def isEmpty(seq : Seq[A]) = seq.isEmpty
    }

  implicit def polarityHasEmpty[A : HasEmpty] : HasEmpty[Polarity[A]] = 
    new HasEmpty[Polarity[A]] {
      def empty = Neutral(implicitly[HasEmpty[A]].empty)
    }

}

sealed trait Expression[A] {

  def ident : IndexedIdentifier[A]
  def isThin : Boolean

  def styleString : String

  def id = ident.toString

  def map[B : HasEmpty](f : A => B) : Expression[B]

  override def toString = id

}

case class Variable[A : HasEmpty](val shell : NCell[A], val ident : IndexedIdentifier[A], val isThin : Boolean) extends Expression[A] {

  def map[B : HasEmpty](f : A => B) = Variable(shell map f, ident map f, isThin)

  def styleString = if (isThin) "var-thin" else "var"

}

case class Interior[A : HasEmpty](val bdry : A, val nook : NCell[A], val ident : IndexedIdentifier[A]) extends Expression[A] {

  def map[B : HasEmpty](f : A => B) = Interior(f(bdry), nook map f, ident map f)
  def isThin : Boolean = true

  def styleString = "filler"
}

case class Boundary[A : HasEmpty](val intr : A, val ident : IndexedIdentifier[A], val isThin : Boolean) extends Expression[A] {

  def map[B : HasEmpty](f : A => B) = Boundary(f(intr), ident map f, isThin)
  def styleString = if (isThin) "filler-face-thin" else "filler-face"
}

case class Contraction[A : HasEmpty](val shell : NCell[A], val ident : IndexedIdentifier[A]) extends Expression[A] {
  def map[B : HasEmpty](f : A => B) = Contraction(shell map f, ident map f)
  def isThin : Boolean = true
  def styleString = "ufiller"
}

case class Application[A : HasEmpty](val defn : Definition, val bindings : Seq[A], val shell : NCell[A]) extends Expression[A] {

  def map[B : HasEmpty](f : A => B) = Application(defn, bindings map f, shell map f)
  def ident = ???
  def isThin = ???

  def styleString = "app"

}
