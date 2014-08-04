/**
  * ModuleChecker.scala - The module level typechecker
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.typechecker

import scalaz._
import Kleisli._

import orchard.core.cell._
import orchard.core.util._
import ErrorM._
import MonadUtils._

trait ModuleChecker extends Checker with CheckerModuleSystem with CheckerWorksheets {

  type ModuleCheckerEnv = ModuleZipper
  type ModuleCheckerM[+A] = Kleisli[Error, ModuleCheckerEnv, A]
  type ModuleCheckerR[E, A] = Kleisli[Error, E, A]

  val MC = MonadReader[ModuleCheckerR, ModuleCheckerEnv]
  import MC._

  def toRoot : ModuleCheckerM[ModuleZipper] = 
    for {
      cursor <- ask
    } yield ModuleZipper(cursor.zip, Nil)

  def seek(addr : Vector[Int]) : ModuleCheckerM[ModuleZipper] = 
    for {
      root <- toRoot
      zipper <- moduleError(root.seek(addr))
    } yield zipper

  def localEnvironment : ModuleCheckerM[Vector[ModuleEntry]] =
    for {
      cursor <- ask
    } yield {
      cursor.context match {
        case Nil => Vector.empty
        case c :: _ => c.left
      }
    }

  def environment : ModuleCheckerM[Vector[ModuleEntry]] =
    for {
      cursor <- ask
    } yield cursor.upperSlice :+ (cursor.focus)

  def modulesInScope : ModuleCheckerM[Vector[Module]] = 
    for {
      env <- environment
    } yield {
      (env map {
        case m : Module => Some(m)
        case _ => None
      }).flatten
    }

  def environmentWithAddresses : ModuleCheckerM[Vector[(ModuleEntry, Vector[Int])]] = 
    for {
      cursor <- ask
    } yield cursor.upperSliceWithAddress :+ (cursor.focus, cursor.toAddress)

  def modulesWithAddresses : ModuleCheckerM[Vector[(Module, Vector[Int])]] = 
    for {
      envAddrs <- environmentWithAddresses
    } yield {
      (envAddrs map {
        case (m : Module, addr) => Some((m, addr))
        case _ => None
      }).flatten
    }


  def findModuleByName(moduleName : String) : ModuleCheckerM[(Module, Vector[Int])] = ???

  def insertModule(moduleName : String) : ModuleCheckerM[ModuleZipper] = 
    for {
      cursor <- ask
      result <- moduleError(
        cursor.insertAfter(
          Module(new ModuleDefinition(moduleName, Vector.empty), Vector.empty)
        )
      )
    } yield result

  def insertParameter(identifier : RawIdentifier) : ModuleCheckerM[ModuleZipper] = ???
  def insertDefinition(boundaryIdentifier : RawIdentifier) : ModuleCheckerM[ModuleZipper] = ???


  def focusAsModule : ModuleCheckerM[Module] = 
    for {
      cursor <- ask
      _ <- moduleError(
        ensure(
          cursor.focus.isInstanceOf[Module],
          "Focus is not a module."
        )
      )
    } yield cursor.focus.asInstanceOf[Module]

  def appendParameter(
    identifier : RawIdentifier, 
    worksheetId : Int, 
    address : CellAddress
  ) : ModuleCheckerM[ModuleZipper] =
    ???

  // Okay, I want to take a crack at this.

  def insertImport(importName : String, moduleName : String) : ModuleCheckerM[ModuleZipper] = 
    for {
      ma <- findModuleByName(moduleName)
      (module, addr) = ma
      modZipper <- seek(addr)

      _ <- moduleError(
        ensure(
          module.entries.length > 0,
          "Refusing to import empty module"
        )
      )

      modLastZipper <- moduleError(
        modZipper.visitEntry(module.entries.length - 1)
      )

      modEnv <- local(_ => modLastZipper)(environmentWithAddresses)

      // The following is pretty wrong, but just to get started
      importDefn = new ImportDefinition(importName, module)
      importEntries = modEnv map {
        // case (Module(m), entryAddr) => Module(new ImportedModule(m))
        // case (Import(i), entryAddr) => Import(new ImportedImport(i))
        case (Parameter(p), entryAddr) => Parameter(new ImportedParameter(p))
        case (Definition(d), entryAddr) => Definition(new ImportedDefinition(d))
        case (_, entryAddr) => ???
      }

      cursor <- ask
      result <- moduleError(
        cursor.insertAfter(
          Import(importDefn, importEntries)
        )
      )
    } yield result


  // Right, now what?

  import scala.math.Ordering.Implicits._

  // The above gives the dictionary ordering on vectors.  How do I recognize something that is
  // also in my scope?

  //============================================================================================
  // ERROR HELPERS
  //

  def moduleError[A](e : Error[A]) : ModuleCheckerM[A] = kleisli(env => e)

  def moduleSucceed[A](a : A) : ModuleCheckerM[A] = moduleError(success(a))
  def moduleFail[A](msg : String) : ModuleCheckerM[A] = moduleError(fail(msg))

}
