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

          parameterName <- runScoped(ident.expand)
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
          cursor <- insertEntry(parameter)
          result <- scope(cursor)(check(next(parameter)))

        } yield result

      case Free(CreateDefinition(ident, nook, next)) =>
        for {

          definitionName <- runScoped(ident.expand)
          qualifiedName <- qualifyName(definitionName)

          defnNook = new Nook(nook)
          isExposed <- runScoped(defnNook.framework.isExposedNook)

          _ <- attempt(
            ensure(
              isExposed,
              "Cannot create definition: not an exposed nook"
            )
          )

          definitionNode = new DefinitionNode(qualifiedName, Filler(ident, defnNook))
          definition = Definition(definitionNode)
          cursor <- insertEntry(definition)
          result <- scope(cursor)(check(next(definition)))

        } yield result


      case Return(a) => succeed(a)

      case _ => fail("Unimplemented: " ++ stmtBlk.toString)

    }


  def runScoped[A](cmd : InScope[A]) : Checker[A] = 
    for {
      scp <- localScope
      result <- attempt(
        cmd(scp)
      )
    } yield result

  // Also, when encountering the end of a collection of siblings, we should
  // not move to the parent, but to the entry before us in the parent.  This
  // can be called "uncle" or something ...
  def localScope : Checker[Scope] = 
    for {
      cursor <- ask

      siblingScope <- cursor.leftSibling match {
        case Left(_) => 
          for {
            // Ooops, no. This is still wrong.  If there is no uncle,
            // we should try the grandparent, etc.  Have to think of a different way ...
            enclosingScope <- cursor.uncle match {
              case Left(_) => succeed(emptyScope)
              case Right(u) => scope(u)(localScope)
            }
          } yield enclosingScope
        case Right(l) => scope(l)(localScope)
      }

      // Now we need to look at the local entry and add it to the scope
      thisEntry = cursor.focus match {
        case m : Module => Map(
          m.node.qualifiedName.toString -> Right(m)
        )
        case p : Parameter => Map(
          p.node.qualifiedName.toString -> Left(p.parameterNode.variable)
        )
        case d : Definition => Map(
          d.node.qualifiedName.toString -> Left(d.definitionNode.filler.Boundary),
          "def-" ++ d.node.qualifiedName.toString -> Left(d.definitionNode.filler)  // Well, this is wrong.  We need a method for this
        )
        case _ => ???
      }
    } yield siblingScope ++ thisEntry

  //============================================================================================
  // UTILITIES
  //

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
      result <- cursor.parent match {
        case Left(_) => succeed(name)
        case Right(p) => scope(p)(qualifyName(ScopedName(containerName, name)))
      }
    } yield result

  //============================================================================================
  // ERROR LIFTING
  //

  def attempt[A](e : Error[A]) : Checker[A] = kleisli(env => e)
  def succeed[A](a : A) : Checker[A] = point(a)
  def fail[A](msg : String) : Checker[A] = attempt(errorFail(msg))

}

object SimpleChecker extends TypeChecker
