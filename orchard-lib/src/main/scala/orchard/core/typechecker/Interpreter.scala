/**
  * Interpreter.scala - Interpreter for opetopic commands
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.typechecker

import scala.language.higherKinds

import scalaz._
import StateT._

import orchard.core.util._
import ErrorM._
import MonadUtils._

trait Interpreter 
    extends TypeChecker 
    with ModuleEntries
    with Syntax 
    with Examples {

  // Temporary ...
  class ModuleZipper

  case class InterpreterState(
    val cursor : ModuleZipper
  )

  type Cmd[+A] = StateT[Error, InterpreterState, A]
  type CmdT[M[+_], A] = StateT[M, InterpreterState, A]
  type CmdS[S, A] = StateT[Error, S, A]

  object InterpreterErrorSyntax extends ErrorLifts[CmdT]
  import InterpreterErrorSyntax._

  val CmdState = MonadState[CmdS, InterpreterState]
  import CmdState._

  // Now, we have the command monad which lives outside the type checker.

  def getCursor : Cmd[ModuleZipper] = 
    for {
      st <- get
    } yield st.cursor

  // def activeModule : Cmd[Module] = 
  //   for {
  //     cursor <- getCursor
  //     module <- attempt(
  //       cursor.focusAsModule
  //     )
  //   } yield module

}

object SimpleInterpreter extends Interpreter
