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

  sealed trait ModuleEntry
  case class Module(name : String, parent : Option[Module], entries : Vector[ModuleEntry]) extends ModuleEntry
  case class ExpressionEntry(expr : Expression) extends ModuleEntry

  case class CheckerState(
    val activeModule : Option[Module],
    val environment : Environment
  )

  type CheckerM[+A] = StateT[Error, CheckerState, A]
  type CheckerS[S, +A] = StateT[Error, S , A]

  val M = MonadState[CheckerS, CheckerState]

  // Okay, well, these leave quite a lot to be desired.  They don't do any semantics and they
  // return the unfolded versions.  But it's a start ...

  // Right, but I think we are going to need much more than just an environment, seeing as how
  // we want to have modules and qualified names and all that jazz.  So how can we do that?

  // I guess we just change the state type to include a kind of reference to the currently 
  // selected module and allow the type checker to modify it's contents.  Why am I feeling
  // lathargic about this right now?  It's a perfect chance to get some shit done.

  def createParameter(name : String, shell : NCell[Option[String]]) : CheckerM[Expression] = ???
    // for {
    //   env <- M.get
    //   exprShell <- NCell.sequence(shell map (faceIdentifierLookup(env, _)))
    //   _ <- ensure(! env.isDefinedAt(name), "Duplicate identifier: " ++ name)
    //   _ <- M.put (env + (name -> Variable(name, exprShell)))
    // } yield Reference(name)

  def createLift(name : String, nook : NCell[Option[String]]) : CheckerM[Expression] = ???
    // for {
    //   env <- M.get
    //   exprNook <- NCell.sequence(nook map (faceIdentifierLookup(env, _)))
    //   _ <- ensure(! env.isDefinedAt(name), "Duplicate identifier: " ++ name)
    //   _ <- M.put (env + (name -> Filler(name, exprNook)))
    // } yield Reference(name)

  def faceIdentifierLookup(env : Environment, idOpt : Option[String]) : CheckerM[Option[Expression]] = 
    idOpt match {
      case None => checkerSucceed(None)
      case Some(id) => 
        for {
          expr <- findInEnvironment(env, id)
        } yield Some(expr)
    }

  def beginModule(name : String) : CheckerM[Unit] =
    for {
      st <- M.get
      _ <- setActiveModule(new Module(name, st.activeModule, Vector.empty))
    } yield ()

  def endModule(name : String) : CheckerM[Unit] =
    for {
      st <- M.get
      mod <- getOption(st.activeModule, "Cannot close module " ++ name ++ " because no module is active.")
      _ <- ensure(mod.name == name, "Cannot close module " ++ name ++ " because active module is " ++ mod.name)
      _ <- M.put(CheckerState(mod.parent, st.environment)) 
      // Hmm. the environment should change, since all of the definitions in the last module are now hidden.
      // How to reflect this?
    } yield ()

  // Hmm. So it seems to me that most eveything is computable from simple a pointer to the current module.
  // The environment as well.

  //============================================================================================
  // GETTERS AND SETTERS
  //

  def getEnvironment : CheckerM[Environment] = 
    for {
      st <- M.get
    } yield st.environment

  def setEnvironment(env : Environment) : CheckerM[Unit] = 
    for {
      st <- M.get
      _ <- M.put(CheckerState(st.activeModule, env))
    } yield ()

  // def getRootModule : CheckerM[Module] = 
  //   for {
  //     st <- M.get
  //     mod <- getOption(st.rootModule, "No root module.")
  //   } yield mod

  def getActiveModule : CheckerM[Module] = 
    for {
      st <- M.get
      mod <- getOption(st.activeModule, "No active module.")
    } yield mod

  def setActiveModule(mod : Module) : CheckerM[Unit] = 
    for {
      st <- M.get
      _ <- M.put(CheckerState(Some(mod), st.environment))
    } yield ()

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

  def getOption[A](opt : Option[A], noneStr : String) : CheckerM[A] = 
    opt match {
      case None => checkerFail(noneStr)
      case Some(a) => checkerSucceed(a)
    }

  implicit def liftError[A](e : Error[A]) : CheckerM[A] =
    StateT[Error, CheckerState, A]((st : CheckerState) => { e map (a => (st, a)) })
}
