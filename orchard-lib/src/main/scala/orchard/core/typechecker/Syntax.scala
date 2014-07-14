/**
  * Syntax.scala - Raw Syntax for Opetopic Language
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.typechecker

import orchard.core.cell._

import scalaz._

sealed trait Statement[+A]
case class ParameterDec[+A](name : String, next : A) extends Statement[A]
case class LiftDec[+A](name : String, next : A) extends Statement[A]
case object Done extends Statement[Nothing]

object Statement {

  implicit def isFunctor : Functor[Statement] = 
    new Functor[Statement] {

      def map[A, B](fa : Statement[A])(f : A => B) : Statement[B] = 
        fa match {
          case ParameterDec(name, next) => ParameterDec(name, f(next))
          case LiftDec(name, next) => LiftDec(name, f(next))
          case Done => Done
        }

    }

  // Now, I want to use the free monad.

  type FreeM[A] = Free[Statement, A]
  // val M = Monad[FreeM]
  import Free._

  def paramDec(name : String) : FreeM[Unit] = 
    Suspend(ParameterDec(name, Return(())))

  def liftDec(name : String) : FreeM[Unit] = 
    Suspend(LiftDec(name, Return(())))

  def test : FreeM[Unit] = 
    for {
      _ <- paramDec("Eric")
      _ <- liftDec("Fred")
    } yield ()

}
