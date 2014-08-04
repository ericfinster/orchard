/**
  * CheckerScoped.scala - Scoped Computations
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

trait CheckerScoped { thisChecker : Checker =>

  type Scoped[+A] = Kleisli[Error, ModuleEntry, A]

  val ScopedReader = MonadReader[ErrorEnvironment, ModuleEntry]
  import ScopedReader._

  type EnvironmentKey

  def lookup(key : EnvironmentKey) : Scoped[Expression] = ???

  //============================================================================================
  // ERROR LIFTING
  //

  def scopedError[A](e : Error[A]) : Scoped[A] = kleisli(env => e)
  def scopedSucceed[A](a : A) : Scoped[A] = point(a)
  def scopedFail[A](msg : String) : Scoped[A] = scopedError(fail(msg))

}
