/**
  * Experiments.scala - Some experiments
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

import scalaz._
import std.list._
import std.vector._
import std.option._
import scalaz.Id._
import scalaz.State._
import scalaz.StateT._
import scalaz.Validation._
import scalaz.\/._
import syntax.traverse._
import Kleisli._

object Experimental { 

  sealed trait Statement
  case class ModuleDec(name : String, body : List[Statement]) extends Statement
  case class VariableDec(name : String) extends Statement
  case class VariableRef(name : String) extends Statement

  sealed trait ModuleEntry
  case class Module(name : String, body : List[ModuleEntry]) extends ModuleEntry
  case class Parameter(name : String) extends ModuleEntry
  case class Reference(moduleEntry : ModuleEntry) extends ModuleEntry

  val test1 =
    ModuleDec("Main", List(
      VariableDec("x"),
      VariableDec("y"),
      VariableDec("z"),
      ModuleDec("Submod1", List(
        VariableDec("A"),
        VariableRef("x"),
        VariableDec("B")))))

  val test2 = 
    ModuleDec("Main", List(
      VariableDec("x"),
      VariableDec("y"),
      VariableDec("z"),
      ModuleDec("Submod1", List(
        VariableDec("A"),
        VariableRef("f"),
        VariableDec("B")))))


  type V[+T] = String \/ T

  type EnvType = Map[String, ModuleEntry]
  type EntryBuilder[A] = StateT[V, EnvType, A]
  type EntryReader[S, A] = StateT[V, S, A]

  val R = MonadState[EntryReader, EnvType]
  import R._

  def success[A](a : A) : V[A] = \/-(a)
  def failure(cause : String) : V[Nothing] = -\/(cause)

  def stateSuccess[A](a : A) : EntryBuilder[A] = point(a)
  def stateFailure[A](str : String) : EntryBuilder[A] = StateT(_ => failure(str))

  def liftK[A](e : V[A]) : EntryBuilder[A] = 
    StateT(st => insertState(st, e))

  def insertState[A](st : EnvType, e : V[A]) : V[(EnvType, A)] = e map (a => (st, a))

  def buildEntry(stmt : Statement) : EntryBuilder[ModuleEntry] = 
    stmt match {
      case ModuleDec(name, body) => 
        for {
        entries <- (body map buildEntry).sequence
        } yield Module(name, entries)
      case VariableDec(name) => 
        for {
          env <- get
          newParam = Parameter(name)
          _ <- put(env + (name -> newParam))
        } yield newParam
      case VariableRef(name) =>
        for {
          env <- get
          refdModule <- liftK(findReference(name, env))
        } yield Reference(refdModule)

    }

  def findReference(name : String, env : Map[String, ModuleEntry]) : V[ModuleEntry] = 
    if (env.isDefinedAt(name))
      success(env(name))
    else
      failure("Unresolved reference: " ++ name)

  val successTest = buildEntry(test1)(Map.empty)
  val failureTest = buildEntry(test2)(Map.empty)

  // Hmmm.  But now we need to move on to the next guy with the previous guy in the environment.
  // It seems what I really want to do is to append these guys as I go.  So it's like I need to
  // look at the current module and append this guy to the environment.  Or can I just return
  // this updated guy?  Not in the reader monad.  That's the state monad.  Because you don't
  // have all the parameters with a single body, you see them progressively as you go.  So
  // the state should be updated

  // type VNL[+A] = ValidationNel[String, A]
  // type Test[S, A] = StateT[V, S, A]
  // type ModuleBuilder[A] = StateT[V, Module, A]
  // val A = Applicative[ModuleBuilder]
  // val M = Monad[ModuleBuilder]
  // val S = MonadState[Test, Module]

  // import S._

  // Okay, the game is to get from point A to point B with proper error handling and in a nice
  // monadic style.  So the first obvious thing is in fact to use a reader monad and not a state
  // one.  Then we just have make a map with qualified names mapping to the resulting entries ...

}
