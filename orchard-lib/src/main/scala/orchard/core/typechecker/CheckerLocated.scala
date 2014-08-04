/**
  * CheckerLocated.scala - Located Computations
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

trait CheckerLocated { thisChecker : Checker =>

  type Located[+A] = Kleisli[Error, ModuleZipper, A]

  val LocatedReader = MonadReader[ErrorEnvironment, ModuleZipper]

  import LocatedReader._

  def locatedError[A](e : Error[A]) : Located[A] = kleisli(env => e)
  def locatedSuccess[A](a : A) : Located[A] = point(a)
  def locatedFail[A](msg : String) : Located[A] = locatedError(fail(msg))

}
