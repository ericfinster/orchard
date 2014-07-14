/**
  * CheckerMonad.scala - The Monad for checking orchard expressions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.typechecker

import scala.language.implicitConversions

import orchard.core.cell._
import scalaz._

trait CheckerMonad {

  type Error[+A] = \/[String, A]
  type Environment = Map[String, Expression] 
  type CheckerM[+A] = StateT[Error, Environment, A]
  type CheckerS[S, +A] = StateT[Error, S , A]

  val M = MonadState[CheckerS, Environment]
  import M._

  // Okay, well, these leave quite a lot to be desired.  They don't do any semantics and they
  // return the unfolded versions.  But it's a start ...

  // Right, but I think we are going to need much more than just an environment, seeing as how
  // we want to have modules and qualified names and all that jazz.  So how can we do that?

  // I guess we just change the state type to include a kind of reference to the currently 
  // selected module and allow the type checker to modify it's contents.  Why am I feeling
  // lathargic about this right now?  It's a perfect chance to get some shit done.

  def assumeVariable(name : String, shell : NCell[Option[String]]) : CheckerM[Expression] = 
    for {
      env <- get
      exprShell <- NCell.sequence(shell map (faceIdentifierLookup(env, _)))
      _ <- ensure(! env.isDefinedAt(name), "Duplicate identifier: " ++ name)
      _ <- put (env + (name -> Variable(name, exprShell)))
    } yield Reference(name)

  def createLift(name : String, nook : NCell[Option[String]]) : CheckerM[Expression] = 
    for {
      env <- get
      exprNook <- NCell.sequence(nook map (faceIdentifierLookup(env, _)))
      _ <- ensure(! env.isDefinedAt(name), "Duplicate identifier: " ++ name)
      _ <- put (env + (name -> Filler(name, exprNook)))
    } yield Reference(name)

  def faceIdentifierLookup(env : Environment, idOpt : Option[String]) : CheckerM[Option[Expression]] = 
    idOpt match {
      case None => checkerSucceed(None)
      case Some(id) => 
        for {
          expr <- findInEnvironment(env, id)
        } yield Some(expr)
    }

  //============================================================================================
  // ERROR HELPERS
  //

  def checkerSucceed[A](a : A) : Error[A] = \/-(a)
  def checkerFail[A](str : String) : Error[A] = -\/(str)

  def ensure(prop : Boolean, failString : String) : CheckerM[Unit] = 
    if (prop) checkerSucceed(()) else checkerFail(failString)

  def findInEnvironment(env : Environment, name : String) : CheckerM[Expression] =
    if (env.isDefinedAt(name)) 
      checkerSucceed(env(name)) 
    else 
      checkerFail("Environment lookup failed.")

  def nop : CheckerM[Unit] = checkerSucceed(())

  implicit def liftError[A](e : Error[A]) : CheckerM[A] =
    StateT[Error, Environment, A]((env : Environment) => { e map (a => (env, a)) })
}
