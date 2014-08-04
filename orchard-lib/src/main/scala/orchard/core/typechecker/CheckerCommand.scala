/**
  * CheckerCommand.scala - Stateful commands
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.typechecker

import scalaz._

import orchard.core.util._
import ErrorM._
import MonadUtils._

trait CheckerCommand { thisChecker : Checker =>

  type Command[+A] = StateT[Error, ModuleZipper, A]
  type CommandS[S, A] = StateT[Error, S, A]

  val CommandState = MonadState[CommandS, ModuleZipper]
  import CommandState._

  def appendModule(moduleName : String) : Command[Module] = 
    for {
      cursor <- get
      module = Module(new ModuleDefinition(moduleName, Vector.empty), Vector.empty)
      newCursor <- commandTry(cursor.appendToModule(module))
      _ <- put(newCursor)
    } yield module

  //============================================================================================
  // COMMAND ERROR HELPERS
  //

  def commandTry[A](e : Error[A]) : Command[A] = 
    StateT[Error, ModuleZipper, A]((cursor : ModuleZipper) => 
      { e map (a => (cursor, a)) })

  def commandSuccess[A](a : A) : Command[A] = point(a)
  def commandFail[A](msg : String) : Command[A] = commandTry(fail(msg))

}
