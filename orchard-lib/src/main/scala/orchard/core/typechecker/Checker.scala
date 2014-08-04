/**
  * Checker.scala - Core type checking
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.typechecker

import scalaz._
import Kleisli._

import orchard.core.util._
import ErrorM._
import MonadUtils._

trait Checker extends CheckerExpressions with CheckerIdentifiers with CheckerFrameworks {

  type CheckerEnv = Seq[Expression]
  type CheckerM[+A] = Kleisli[Error, CheckerEnv, A]
  type CheckerR[E, A] = Kleisli[Error, E, A]

  type EnvironmentKey

  val R = MonadReader[CheckerR, CheckerEnv]
  import R._

  def lookup(key : EnvironmentKey) : CheckerM[Expression] = ???
    // for {
    //   env <- ask
    //   _ <- liftError(
    //     ensure(
    //       env.isDefinedAt(index),
    //       "Requested environment index " ++ index.toString ++ " is out of range."
    //     )
    //   )
    // } yield env(index)

  def liftError[A](e : Error[A]) : CheckerM[A] = kleisli(env => e)

  def checkerSucceed[A](a : A) : CheckerM[A] = liftError(success(a))
  def checkerFail[A](msg : String) : CheckerM[A] = liftError(fail(msg))


}
