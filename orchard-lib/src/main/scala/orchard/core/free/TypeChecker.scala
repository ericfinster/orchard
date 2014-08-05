/**
  * TypeChecker.scala - A Type Checker for Orchard
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.free

import scalaz.{Free => _, _}
import Kleisli._

import orchard.core.util._
import ErrorM.{success => errorSuccess, fail => errorFail, _}
import MonadUtils._

trait TypeChecker 
    extends Scoped
    with Syntax
    with Expressions
    with Identifiers
    with Frameworks
    with Worksheets
    with ModuleEntries 
    with Examples {

  type ErrorEnvironment[E, A] = Kleisli[Error, E, A]

  type CheckerEnvironment = ModuleZipper
  type Checker[+A] = Kleisli[Error, CheckerEnvironment, A]

  val CheckerReader = MonadReader[ErrorEnvironment, ModuleZipper]
  import CheckerReader._

  //============================================================================================
  // MAIN CHECKING ROUTINE
  //

  import FreeMonad._

  def check[A](stmtBlk : FreeM[A]) : Checker[A] =
    stmtBlk match {

      // Begin module
      case Free(BeginModule(moduleName, next)) =>
        for {
          qualifiedName <- qualifyName(moduleName)
          moduleNode = new ModuleNode(qualifiedName, Vector.empty)
          module = Module(moduleNode, Vector.empty)
          cursor <- appendEntry(module)
          result <- scope(cursor)(check(next))
        } yield result


      // End module
      case Free(EndModule(next)) =>
        for {
          cursor <- ask
          nextCursor <- attempt(
            cursor.parent
          )
          result <- scope(nextCursor)(check(next))
        } yield result


      // Create parameter
      case Free(CreateParameter(ident, shell, isThin, next)) => 
        for {

          modScope <- moduleScope

          parameterName <- runInScope(modScope, ident.expand)
          qualifiedName <- qualifyName(parameterName)

          varShell = new Shell(shell)

          _ <- attempt(
            ensure(
              varShell.framework.isShell,
              "Cannot create parameter " ++ parameterName ++ ": framework is not a shell"
            )
          )

          parameterNode = new ParameterNode(qualifiedName, Variable(ident, varShell, isThin))
          parameter = Parameter(parameterNode)
          entryCursor <- appendEntry(parameter)
          nextCursor <- attempt(entryCursor.parent)
          result <- scope(nextCursor)(check(next(parameter)))

        } yield result

      // Create definition
      case Free(CreateDefinition(ident, nook, next)) =>
        for {

          modScope <- moduleScope

          definitionName <- runInScope(modScope, ident.expand)
          qualifiedName <- qualifyName(definitionName)

          defnNook = new Nook(nook)
          isExposed <- runInScope(modScope, defnNook.framework.isExposedNook)

          _ <- attempt(
            ensure(
              isExposed,
              "Cannot create definition: not an exposed nook"
            )
          )

          definitionNode = new DefinitionNode(qualifiedName, Filler(ident, defnNook))
          definition = Definition(definitionNode)
          entryCursor <- appendEntry(definition)
          nextCursor <- attempt(entryCursor.parent)
          result <- scope(nextCursor)(check(next(definition)))

        } yield result

      // State examination
      case Free(ExamineState(next)) =>
        for {
          cursor <- ask
          result <- check(next(cursor))
        } yield result


      case Free(ExamineModuleScope(next)) =>
        for {
          s <- moduleScope
          result <- check(next(s))
        } yield result

      case Free(ExamineLocalScope(next)) =>
        for {
          s <- localScope
          result <- check(next(s))
        } yield result

      // Success
      case Return(a) => succeed(a)

      case _ => fail("Unimplemented: " ++ stmtBlk.toString)

    }

  //============================================================================================
  // SCOPE MANAGEMENT
  //

  def runInScope[A](scp : Scope, cmd : InScope[A]) : Checker[A] = 
    for {
      result <- attempt(
        cmd(scp)
      )
    } yield result


  def moduleScope : Checker[Scope] = 
    for {
      module <- activeModule
      scp <- localScope
    } yield (scp ++ (module.entries map entryScope).flatten)


  def localScope : Checker[Scope] = 
    localScopeBuilder(false)

  def localScopeBuilder(includeCursor : Boolean) : Checker[Scope] = 
    for {
      cursor <- ask

      siblingScope <- branchOn(cursor.leftSibling)(
        success = l => scope(l)(localScopeBuilder(true)),
        failure = for {
          enclosingScope <- branchOn(cursor.parent)(
            success = p => scope(p)(localScopeBuilder(false)),
            failure = succeed(emptyScope)
          )
        } yield enclosingScope
      )

    } yield {
      siblingScope ++
        (if (includeCursor) entryScope(cursor.focus) else emptyScope)
    }

  def entryScope(entry : ModuleEntry) : Scope =
    entry match {
      case m : Module => Map(
        m.node.qualifiedName.toString -> Right(m)
      )
      case p : Parameter => Map(
        p.node.qualifiedName.toString -> Left(p.parameterNode.variable)
      )
      case d : Definition => Map(
        d.node.qualifiedName.toString -> Left(d.definitionNode.filler.Boundary),
        d.node.fillerQualifiedName.toString -> Left(d.definitionNode.filler)  
      )
      case i : Import => Map.empty
    }

  //============================================================================================
  // UTILITIES
  //


  def activeModule : Checker[Module] = 
    for {
      cursor <- ask
      module <- attempt(
        cursor.focusAsModule
      )
    } yield module

  def appendEntry(entry : ModuleEntry) : Checker[ModuleZipper] =
    for {
      cursor <- ask
      nextCursor <- attempt(
        cursor.appendToModule(entry)
      )
    } yield nextCursor

  def insertEntry(entry : ModuleEntry) : Checker[ModuleZipper] = 
    for {
      cursor <- ask
      nextCursor <- attempt(
        cursor.insertAfter(entry)
      )
    } yield nextCursor

  def qualifyName(name : String) : Checker[QualifiedName] = 
    qualifyName(LocalName(name))

  // This is some kind of or else construction
  def qualifyName(name : QualifiedName) : Checker[QualifiedName] = 
    for {
      cursor <- ask
      containerName = cursor.focus.node.name
      result <- branchOn(cursor.parent)(
        success = p => scope(p)(qualifyName(ScopedName(containerName, name))),
        failure = succeed(name)
      )
    } yield result

  //============================================================================================
  // ERROR LIFTING
  //

  def attempt[A](e : Error[A]) : Checker[A] = kleisli(env => e)
  def succeed[A](a : A) : Checker[A] = point(a)
  def fail[A](msg : String) : Checker[A] = attempt(errorFail(msg))

  def branchOn[A, B](e : Error[A])(success : A => Checker[B], failure : Checker[B]) = 
    e match {
      case Left(_) => failure
      case Right(a) => success(a)
    }

}

object SimpleChecker extends TypeChecker
