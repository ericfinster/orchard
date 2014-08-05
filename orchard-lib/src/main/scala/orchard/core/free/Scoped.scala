/**
  * Scoped.scala - Scoped computations
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.free

import scalaz._
import Kleisli._

import orchard.core.util._
import ErrorM.{success => errorSuccess, fail => errorFail, _}
import MonadUtils._

trait Scoped { thisChecker : TypeChecker =>

  type ScopedEntry = Either[Expression, Module]
  type Scope = Map[String, ScopedEntry]
  type InScope[+A] = Kleisli[Error, Scope, A]

  val ScopedReader = MonadReader[ErrorEnvironment, Scope]
  import ScopedReader._

  def emptyScope : Scope = Map.empty[String, ScopedEntry]

  type EnvironmentKey = String

  def lookup(key : EnvironmentKey) : InScope[Expression] =
    for {
      env <- ask
      entry <- attemptInScope(
        fromOption(
          env.get(key),
          "Not found: " ++ key
        )
      )
      expr <- attemptInScope(
        fromOption(
          entry.left.toOption,
          "Key " ++ key ++ " resovles to module"
        )
      )
    } yield expr

  //============================================================================================
  // ERROR LIFTING
  //

  def attemptInScope[A](e : Error[A]) : InScope[A] = kleisli(env => e)
  def succeedInScope[A](a : A) : InScope[A] = point(a)
  def failInScope[A](msg : String) : InScope[A] = attemptInScope(errorFail(msg))


}
