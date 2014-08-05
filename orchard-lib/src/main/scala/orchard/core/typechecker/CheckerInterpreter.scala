/**
  * CheckerInterpreter.scala - An interpreter
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.typechecker

import scalaz._
import Kleisli._
import Free._

import orchard.core.cell._
import orchard.core.util._

import ErrorM._
import MonadUtils._

trait CheckerInterpreter { thisChecker : Checker with CheckerSyntax =>

  //============================================================================================
  // INTERPRETATION
  //

  // What does the interpretation look like?  I guess it is just a reader monad which maps strings
  // to expressions.

  type InterpEnv = (List[(String, Vector[Worksheet])], Map[String, Expr])
  type Interpreter[+A] = Kleisli[Error, InterpEnv, A]

  val InterpreterM = MonadReader[ErrorEnvironment, InterpEnv]
  import InterpreterM._

  // Right, so this is fucking sick. You interpret the next command in the 
  // extended environment.  What I don't see is how to incorporate modules.

  // How does that work?  Is it just the begin/end statement?

  def interpret[A](stmt : FreeM[A]) : Interpreter[A] = 
    stmt match {
      case Suspend(BeginModuleStatement(moduleName, next)) => 
        for {
          env <- ask
          modQual <- moduleQualifier
          res <- scope((((moduleName, Vector.empty) :: env._1), env._2))(interpret(next))
        } yield res
      case Suspend(EndModuleStatement(moduleName, next)) =>
        for {
          env <- ask
          res <- scope(env._1.tail, env._2)(interpret(next))
        } yield res
      case Suspend(ParameterStatement(ident, shell, isThin, next)) => 
        for {
          env <- ask 
          modQual <- moduleQualifier
          v <- createParameter(ident, shell, isThin)
          res <- scope((env._1, (env._2 + ((modQual.mkString("", ".", ".") ++ v.name) -> v))))(interpret(next(v)))
        } yield res
      case _ => interpFail("Unimplemented")
    }

  def environment : Interpreter[Map[String, Expr]] = 
    for {
      env <- ask
    } yield env._2

  def moduleQualifier : Interpreter[List[String]] = 
    for {
      env <- ask
    } yield env._1.unzip._1

  def createParameter(ident : String, shell : NCell[Option[Expr]], isThin : Boolean) : Interpreter[Var] = 
    interpSuccess(Var(ident, shell, isThin))

  def createBoundary(ident : String, nook : NCell[Option[Expr]]) : Interpreter[Bdry] =
    interpSuccess(Bdry(ident, nook))

  // Well, now the idea is simple, you should check that this guy really is a shell, and if so, right ...

  def interpTry[A](e : Error[A]) : Interpreter[A] = kleisli(env => e)
  def interpSuccess[A](a : A) : Interpreter[A] = point(a)
  def interpFail[A](msg : String) : Interpreter[A] = interpTry(fail(msg))



}
