/**
  * Checker.scala - The main checker class for Orchard
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.checker

import scalaz._

import orchard.core.util._
import ErrorM._
import ErrorMonad._

trait Checker extends CheckerModuleSystem {

  type CheckerM[+A] = StateT[Error, ModuleZipper, A]
  type CheckerS[S, +A] = StateT[Error, S , A]

  val M = MonadState[CheckerS, ModuleZipper]
  import M._

  def getEnvironment(offset : Int) : CheckerM[Vector[String]] = 
    for {
      ptr <- get
    } yield {
      val outerEnvironment = ptr.collectLefts map (_.node.name)

      ptr.focus match {
        case m : Module => outerEnvironment ++ (m.entries.take(offset) map (_.node.name))
        case _ => outerEnvironment
      }
    }

  def insertModule(moduleId : String, offset : Int) : CheckerM[CheckerModuleNode] = 
    for {
      ptr <- get
      env <- getEnvironment(offset)
      _ <- liftError(ensure(!(env contains moduleId), "Identifier already exists."))
      moduleNode = new CheckerModuleNode(moduleId)
      res <- liftError(ptr.insertAt(Module(moduleNode, Vector.empty), offset))
      _ <- put(res)
    } yield moduleNode

  def insertParameter(identString : String) : CheckerM[CheckerParameterNode] = ???

  def liftError[A](e : Error[A]) : CheckerM[A] =
    StateT[Error, ModuleZipper, A]((ptr : ModuleZipper) => { e map (a => (ptr, a)) })

}

object ErrorMonad {

  implicit val errorIsMonad : Monad[Error] =
    new Monad[Error] {
      def point[A](a : => A) : Error[A] = success(a)
      def bind[A, B](fa : Error[A])(f : A => Error[B]) : Error[B] = 
        fa match {
          case Right(a) => f(a)
          case Left(msg) => fail(msg)
        }
    }

}
