/**
  * Polarity.scala - Polarities for use in Cardinal Complexes
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.complex

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
