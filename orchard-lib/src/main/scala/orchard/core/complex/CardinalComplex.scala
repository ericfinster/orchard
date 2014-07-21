/**
  * CardinalComplex.scala - A Cardinal Complex
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.complex

import scala.collection.mutable.ListBuffer

import orchard.core.cell._
import orchard.core.util._

sealed trait Polarity[+A]
case object Positive extends Polarity[Nothing] { override def toString = "+" }
case object Negative extends Polarity[Nothing] { override def toString = "-" }
case class Neutral[A](value : A) extends Polarity[A] { override def toString = value.toString }

object Polarity {

  implicit class PolarityOps[A](p : Polarity[A]) {

    def map[B](f : A => B) : Polarity[B] = 
      p match {
        case Positive => Positive
        case Negative => Negative
        case Neutral(n) => Neutral(f(n))
      }

  }

}

trait CardinalComplex[A] { thisComplex : MutableSkeletalComplex[Polarity[A]] =>

  override type CellType <: CardinalCell

  trait CardinalCell { thisCardinal : MutableSkeletalCell =>

    def isPositive : Boolean =
      item match {
        case Positive => true
        case _ => false 
      }

    def isNegative : Boolean =
      item match {
        case Negative => true
        case _ => false
      }

    def isNeutral : Boolean =
      item match {
        case Neutral(_) => true
        case _ => false
      }

    def isPolarized : Boolean = isPositive || isNegative

    def neutralNCell : NCell[A] = 
      toNCell map (p =>
        p match {
          case Neutral(a) => a
          case _ => throw new IllegalArgumentException("Cell has a non-neutral face!")
        })
  }

}

object CardinalComplex {
  def defaultCardinal[A](item : A) : NCell[Polarity[A]] = 
    Object(Neutral(item)).glob(Positive, Negative)

  def neutralize[D <: Nat, A](cell : Cell[D, A]) : Cell[D, Polarity[A]] =
    cell map (value => Neutral(value))

  def apply[D <: Nat, A](cell : Cell[D, A]) : Cell[S[D], Polarity[A]] = 
    cell match {
      case Object(value, ev) => {
        implicit val isZero = ev
        Composite(Negative, SeedClass(ObjectCell(Neutral(value))), Positive)
      }
      case Composite(value, srcTree, tgtValue, ev) => {
        implicit val hasPred = ev

        val targetCardinal = CardinalComplex(srcTree.target(tgtValue))

        targetCardinal match {
          case Composite(_, Seed(_), _, e) => {
            Composite(Negative, Graft(targetCardinal, Vector(neutralize(cell).corolla)), Positive)
          }
          case Composite(neg, Graft(card, branches, f), pos, e) => {
            Composite(Negative, Graft(targetCardinal, Vector(neutralize(cell).corolla, Leaf(card).asInstanceOf[CellTree[D, Polarity[A]]])), Positive)
          }
        }
      }
    }
}
