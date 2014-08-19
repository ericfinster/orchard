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


  // This is quite obviously wrong.  But we'll refine it later
  def convertible(e0 : Expression, e1 : Expression) : InScope[Boolean] = 
    succeedInScope(e0 == e1)

  def ensureConvertible(e0 : Expression, e1 : Expression) : InScope[Unit] = 
    for {
      areConvertible <- convertible(e0, e1)
      _ <- if (areConvertible) {
        succeedInScope(())
      } else {
        for {
          e0Name <- e0.name
          e1Name <- e1.name
          _ <- failInScope("Match error: expression " ++ e0Name ++ " is not convertible to " ++ e1Name)
        } yield ()
      }
    } yield ()


  //============================================================================================
  // ERROR LIFTING
  //

  def attemptInScope[A](e : Error[A]) : InScope[A] = kleisli(env => e)
  def succeedInScope[A](a : A) : InScope[A] = point(a)
  def failInScope[A](msg : String) : InScope[A] = attemptInScope(errorFail(msg))


}
