/**
  * TypeChecker.scala - Another go at a type checker
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.typechecker

import scalaz._
import Kleisli._

import orchard.core.util._
import ErrorM.{success => errorSuccess, fail => errorFail, _}
import MonadUtils._

trait TypeChecker 
    extends Expressions
    with Frameworks
    with Identifiers {

  type ErrorEnvironment[E, A] = Kleisli[Error, E, A]

  type Environment = (Vector[Expression], Map[String, Int])
  type Checker[+A] = Kleisli[Error, Environment, A]

  val CheckerR = MonadReader[ErrorEnvironment, Environment]
  import CheckerR._

  type EnvironmentKey = Int
  
  def lookup(key : EnvironmentKey) : Checker[Expression] = ???

  //============================================================================================
  // ERROR LIFTING
  //

  def attempt[A](e : Error[A]) : Checker[A] = kleisli(env => e)
  def succeed[A](a : A) : Checker[A] = point(a)
  def fail[A](msg : String) : Checker[A] = attempt(errorFail(msg))

  def branchOn[A, B](e : Error[A])(success : A => B, failure : B) : B = 
    e match {
      case Left(_) => failure
      case Right(a) => success(a)
    }


}
