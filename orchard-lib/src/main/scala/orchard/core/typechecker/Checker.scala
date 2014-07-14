/**
  * Checker.scala - A Type Checker for Orchard
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.typechecker

import scalaz._
import scalaz.concurrent._

class Checker extends CheckerMonad {

  type StatementSeq = Free[Statement, Unit]

  private var environment : Environment = Map.empty

  // The idea is that this will interpret the statements
  // given by the free monad guy inside the type checking monad,
  // the run the result and report back to the outside ...
  def run(seq : StatementSeq) : Option[String] = ???

}
