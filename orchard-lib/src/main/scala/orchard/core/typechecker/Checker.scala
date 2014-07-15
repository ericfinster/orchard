/**
  * Checker.scala - A Type Checker for Orchard
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.typechecker

import scalaz._
import Free._

class Checker extends CheckerMonad {

  type StatementSeq = Free[Statement, Unit]

  private var environment : Environment = Map.empty

  def interpret(seq : StatementSeq) : CheckerM[Unit] =
    seq match {
      case Suspend(BeginModule(name, next)) => 
        for {
          _ <- beginModule(name)
          _ <- interpret(next)
        } yield ()
      case Suspend(EndModule(name, next)) =>
        for {
          _ <- endModule(name)
          _ <- interpret(next)
        } yield ()
      case Suspend(ParameterDec(name, next)) =>
        for {
          _ <- createParameter(name, ???)
          _ <- interpret(next)
        } yield ()
      case Suspend(LiftDec(name, next)) =>
        for {
          _ <- createLift(name, ???)
          _ <- interpret(next)
        } yield ()
    }

}
