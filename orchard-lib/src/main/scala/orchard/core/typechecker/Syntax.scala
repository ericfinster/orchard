/**
  * Syntax.scala - Raw Syntax for Opetopic Language
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.typechecker

import scala.language.implicitConversions
import orchard.core.cell._

import scalaz._
import Free._

sealed trait Statement[+A] {

  def map[B](f : A => B) : Statement[B] = 
    this match {
      case BeginModule(name, next) => BeginModule(name, f(next))
      case EndModule(name, next) => EndModule(name, f(next))
      case ParameterDec(name, next) => ParameterDec(name, f(next))
      case LiftDec(name, next) => LiftDec(name, f(next))
    }

}

case class BeginModule[+A](name : String, next : A) extends Statement[A]
case class EndModule[+A](name : String, next : A) extends Statement[A]
case class ParameterDec[+A](name : String, next : A) extends Statement[A]
case class LiftDec[+A](name : String, next : A) extends Statement[A]

object Statement {

  implicit def isFunctor : Functor[Statement] = 
    new Functor[Statement] {

      def map[A, B](fa : Statement[A])(f : A => B) : Statement[B] = 
        fa map f

    }

  // Now, I want to use the free monad.

  type FreeM[A] = Free[Statement, A]

  def liftF[A](stmt : Statement[A]) : FreeM[A] =
    Suspend(stmt map (Return(_)))

  def beginModule(name : String) : FreeM[Unit] =
    liftF(BeginModule(name, ()))

  def endModule(name : String) : FreeM[Unit] =
    liftF(EndModule(name, ()))

  def paramDec(name : String) : FreeM[Unit] = 
    liftF(ParameterDec(name, ()))

  def liftDec(name : String) : FreeM[Unit] = 
    liftF(LiftDec(name, ()))

}
