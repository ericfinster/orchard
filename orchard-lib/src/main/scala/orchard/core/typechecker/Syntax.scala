/**
  * Syntax.scala - Syntax for the type checker
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.typechecker

import scala.language.higherKinds

import scalaz.{Free => _, _}

import orchard.core.cell._
import orchard.core.util._

// import ErrorM._
// import MonadUtils._

trait Syntax { thisInterpreter : Interpreter =>

  //============================================================================================
  // INTERPRETER COMMANDS
  //

  sealed trait Command[+A]

  /* Modules */
  case class BeginModule[A](moduleName : String, next : A) extends Command[A]
  case class EndModule[A](next : A) extends Command[A]

  /* Expression Creation */
  case class CreateParameter[A](identString : String, shell : NCell[Option[String]], isThin : Boolean, next : Parameter => A) extends Command[A]
  case class CreateDefinition[A](identString : String, nook : NCell[Option[String]], next : Definition => A) extends Command[A]

  implicit def commandIsFunctor : Functor[Command] = 
    new Functor[Command] {

      def map[A, B](fa : Command[A])(f : A => B) : Command[B] = 
        fa match {
          case BeginModule(moduleName, next) => BeginModule(moduleName, f(next))
          case EndModule(next) => EndModule(f(next))
          case CreateParameter(identString, shell, isThin, next) => CreateParameter(identString, shell, isThin, (v => f(next(v))))
          case CreateDefinition(identString, nook, next) => CreateDefinition(identString, nook, (b => f(next(b))))
        }
    }

  type FreeM[A] = Free[Command, A]
  import Free._

  def liftFree[F[+_], A](fa : F[A])(implicit ev : Functor[F]) : Free[F, A] =
    Join(ev.map(fa)(Return[F, A](_)))

  def beginModule(moduleName : String) : FreeM[Unit] = 
    liftFree(BeginModule(moduleName, ()))

  def endModule : FreeM[Unit] = 
    liftFree(EndModule(()))

  def parameter(identString : String, shell : NCell[Option[String]], isThin : Boolean) : FreeM[Parameter] =
    liftFree(CreateParameter(identString, shell, isThin, identity))

  def definition(identString : String, nook : NCell[Option[String]]) : FreeM[Definition] =
    liftFree(CreateDefinition(identString, nook, identity))

  // //============================================================================================
  // // STATEMENTS
  // //

  // sealed trait Statement[+A]

  // /* Modules */
  // case class BeginModule[A](moduleName : String, next : A) extends Statement[A]
  // case class EndModule[A](next : A) extends Statement[A]

  // /* Expression Creation */
  // case class CreateParameter[A](identifier : Identifier, shell : NCell[FrameworkEntry], isThin : Boolean, next : Parameter => A) extends Statement[A]
  // case class CreateDefinition[A](identifier : Identifier, nook : NCell[FrameworkEntry], next : Definition => A) extends Statement[A]
  // case class ShapeOf[A](expr : Expression, next : NCell[Expression] => A) extends Statement[A]

  // /* State inspection */
  // case class ExamineLocalScope[A](next : Scope => A) extends Statement[A]
  // case class ExamineModuleScope[A](next : Scope => A) extends Statement[A]
  // case class ExamineState[A](next : ModuleZipper => A) extends Statement[A]

  // /* Worksheets */
  // case class WorksheetHandle(val index : Int)
  // case class CreateWorksheet[A](seed : NCell[FrameworkEntry], next : WorksheetHandle => A) extends Statement[A]
  // case class CreateWorksheetWithExpression[A](expr : Expression, next : WorksheetHandle => A) extends Statement[A]
  // case class SelectCellAsBase[A](handle : WorksheetHandle, addr : CellAddress, next : A) extends Statement[A]
  // case class SelectTo[A](handle : WorksheetHandle, addr : CellAddress, next : A) extends Statement[A]
  // case class SelectRay[A](handle : WorksheetHandle, ray : Vector[Int], next : A) extends Statement[A]
  // case class TargetAddress[A](handle : WorksheetHandle, addr : CellAddress, next : CellAddress => A) extends Statement[A]
  // case class Extrude[A](handle : WorksheetHandle, next : Function[(CellAddress, CellAddress), A]) extends Statement[A]
  // case class Drop[A](handle : WorksheetHandle, next : Function[(CellAddress, CellAddress), A]) extends Statement[A]
  // case class Paste[A](handle : WorksheetHandle, addr : CellAddress, expr : Expression, next : A) extends Statement[A]
  // case class ExtractCell[A](handle : WorksheetHandle, addr : CellAddress, next : NCell[FrameworkEntry] => A) extends Statement[A]

  // implicit def statementIsFunctor : Functor[Statement] = 
  //   new Functor[Statement] {

  //     def map[A, B](fa : Statement[A])(f : A => B) : Statement[B] = 
  //       fa match {
  //         case BeginModule(moduleName, next) => BeginModule(moduleName, f(next))
  //         case EndModule(next) => EndModule(f(next))
  //         case CreateParameter(identifier, shell, isThin, next) => CreateParameter(identifier, shell, isThin, (v => f(next(v))))
  //         case CreateDefinition(identifier, nook, next) => CreateDefinition(identifier, nook, (b => f(next(b))))
  //         case ShapeOf(expr, next) => ShapeOf(expr, ncell => f(next(ncell)))
  //         case ExamineLocalScope(next) => ExamineLocalScope(s => f(next(s)))
  //         case ExamineModuleScope(next) => ExamineModuleScope(s => f(next(s)))
  //         case ExamineState(next) => ExamineState(s => f(next(s)))
  //         case CreateWorksheet(seed, next) => CreateWorksheet(seed, w => f(next(w)))
  //         case CreateWorksheetWithExpression(expr, next) => CreateWorksheetWithExpression(expr, w => f(next(w)))
  //         case SelectCellAsBase(handle, addr, next) => SelectCellAsBase(handle, addr, f(next))
  //         case SelectTo(handle, addr, next) => SelectTo(handle, addr, f(next))
  //         case SelectRay(handle, ray, next) => SelectRay(handle, ray, f(next))
  //         case TargetAddress(handle, addr, next) => TargetAddress(handle, addr, tgtAddr => f(next(tgtAddr)))
  //         case Extrude(handle, next) => Extrude(handle, addrPair => f(next(addrPair)))
  //         case Drop(handle, next) => Drop(handle, addrPair => f(next(addrPair)))
  //         case Paste(handle, addr, expr, next) => Paste(handle, addr, expr, f(next))
  //         case ExtractCell(handle, addr, next) => ExtractCell(handle, addr, ncell => f(next(ncell)))
  //       }

  //   }


  // type FreeM[A] = FreeMonad[Statement, A]
  // import FreeMonad._

  // def liftFree[F[+_], A](fa : F[A])(implicit ev : Functor[F]) : FreeMonad[F, A] =
  //   Free(ev.map(fa)(Return[F, A](_)))

  // def beginModule(moduleName : String) : FreeM[Unit] = 
  //   liftFree(BeginModule(moduleName, ()))

  // def endModule : FreeM[Unit] = 
  //   liftFree(EndModule(()))

  // def parameter(identifier : Identifier, shell : NCell[FrameworkEntry], isThin : Boolean) : FreeM[Parameter] =
  //   liftFree(CreateParameter(identifier, shell, isThin, identity))

  // def definition(identifier : Identifier, nook : NCell[FrameworkEntry]) : FreeM[Definition] =
  //   liftFree(CreateDefinition(identifier, nook, identity))

  // def shapeOf(expr : Expression) : FreeM[NCell[Expression]] = 
  //   liftFree(ShapeOf(expr, identity))

  // def emptyWorksheet : FreeM[WorksheetHandle] = 
  //   liftFree(CreateWorksheet(Object(Empty), identity))

  // def worksheetWithExpression(expr : Expression) : FreeM[WorksheetHandle] = 
  //   liftFree(CreateWorksheetWithExpression(expr, identity))

  // def selectAsBase(handle : WorksheetHandle, addr : CellAddress) : FreeM[Unit] = 
  //   liftFree(SelectCellAsBase(handle, addr, ()))

  // def selectTo(handle : WorksheetHandle, addr : CellAddress) : FreeM[Unit] = 
  //   liftFree(SelectTo(handle, addr, ()))

  // def selectRay(handle : WorksheetHandle, ray : Vector[Int]) : FreeM[Unit] =
  //   liftFree(SelectRay(handle, ray, ()))

  // def targetAddress(handle : WorksheetHandle, addr : CellAddress) : FreeM[CellAddress] = 
  //   liftFree(TargetAddress(handle, addr, identity))

  // def extract(handle : WorksheetHandle, addr : CellAddress) : FreeM[NCell[FrameworkEntry]] =
  //   liftFree(ExtractCell(handle, addr, identity))

  // def extrude(handle : WorksheetHandle) : FreeM[(CellAddress, CellAddress)] =
  //   liftFree(Extrude(handle, identity)) 

  // def drop(handle : WorksheetHandle) : FreeM[(CellAddress, CellAddress)] = 
  //   liftFree(Drop(handle, identity)) 

  // def paste(handle : WorksheetHandle, addr : CellAddress, expr : Expression) : FreeM[Unit] = 
  //   liftFree(Paste(handle, addr, expr, ()))

  // // Debug and state examination
  // def examineLocalScope : FreeM[Scope] = 
  //   liftFree(ExamineLocalScope(identity))

  // def examineModuleScope : FreeM[Scope] = 
  //   liftFree(ExamineModuleScope(identity))

  // def examineState : FreeM[ModuleZipper] = 
  //   liftFree(ExamineState(identity))

}
