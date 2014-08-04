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

trait Checker 
    extends CheckerExpressions 
    with CheckerIdentifiers 
    with CheckerFrameworks 
    with CheckerWorksheets 
    with CheckerModuleSystem 
    with CheckerScoped
    with CheckerLocated 
    with CheckerCommand {

  type ErrorEnvironment[E, A] = Kleisli[Error, E, A]

}
