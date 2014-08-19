/**
  * Syntax.scala - Syntax for the type checker
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.free

import scala.language.higherKinds

import scalaz.{Free => _, _}

import orchard.core.cell._
import orchard.core.util._

// import ErrorM._
// import MonadUtils._

trait Syntax { thisChecker : TypeChecker =>

  //============================================================================================
  // QUALIFIED NAMES
  //

  sealed trait QualifiedName 
  case class LocalName(name : String) extends QualifiedName { override def toString = name }
  case class ScopedName(scope : String, name : QualifiedName) extends QualifiedName {
    override def toString = scope ++ "." ++ name.toString
  }

  implicit class QualifiedOps(name : QualifiedName) {

    def localName : String = 
      name match {
        case LocalName(n) => n
        case ScopedName(_, n) => n.localName
      }

    def mapLocal(f : String => String) : QualifiedName =
      name match {
        case LocalName(n) => LocalName(f(n))
        case ScopedName(s, n) => ScopedName(s, n mapLocal f)
      }

  }

  //============================================================================================
  // STATEMENTS
  //

  sealed trait Statement[+A]

  /* Modules */
  case class BeginModule[A](moduleName : String, next : A) extends Statement[A]
  case class EndModule[A](next : A) extends Statement[A]

  /* Expression Creation */
  case class CreateParameter[A](identifier : Identifier, shell : NCell[FrameworkEntry], isThin : Boolean, next : Parameter => A) extends Statement[A]
  case class CreateDefinition[A](identifier : Identifier, nook : NCell[FrameworkEntry], next : Definition => A) extends Statement[A]
  case class ShapeOf[A](expr : Expression, next : NCell[Expression] => A) extends Statement[A]

  /* State inspection */
  case class ExamineLocalScope[A](next : Scope => A) extends Statement[A]
  case class ExamineModuleScope[A](next : Scope => A) extends Statement[A]
  case class ExamineState[A](next : ModuleZipper => A) extends Statement[A]

  /* Worksheets */
  case class WorksheetHandle(val index : Int)
  case class CreateWorksheet[A](seed : NCell[FrameworkEntry], next : WorksheetHandle => A) extends Statement[A]
  case class CreateWorksheetWithExpression[A](expr : Expression, next : WorksheetHandle => A) extends Statement[A]
  case class SelectCellAsBase[A](handle : WorksheetHandle, addr : CellAddress, next : A) extends Statement[A]
  case class SelectTo[A](handle : WorksheetHandle, addr : CellAddress, next : A) extends Statement[A]
  case class SelectRay[A](handle : WorksheetHandle, ray : Vector[Int], next : A) extends Statement[A]
  case class TargetAddress[A](handle : WorksheetHandle, addr : CellAddress, next : CellAddress => A) extends Statement[A]
  case class Extrude[A](handle : WorksheetHandle, next : Function[(CellAddress, CellAddress), A]) extends Statement[A]
  case class Drop[A](handle : WorksheetHandle, next : Function[(CellAddress, CellAddress), A]) extends Statement[A]
  case class Paste[A](handle : WorksheetHandle, addr : CellAddress, expr : Expression, next : A) extends Statement[A]
  case class ExtractCell[A](handle : WorksheetHandle, addr : CellAddress, next : NCell[FrameworkEntry] => A) extends Statement[A]

  implicit def statementIsFunctor : Functor[Statement] = 
    new Functor[Statement] {

      def map[A, B](fa : Statement[A])(f : A => B) : Statement[B] = 
        fa match {
          case BeginModule(moduleName, next) => BeginModule(moduleName, f(next))
          case EndModule(next) => EndModule(f(next))
          case CreateParameter(identifier, shell, isThin, next) => CreateParameter(identifier, shell, isThin, (v => f(next(v))))
          case CreateDefinition(identifier, nook, next) => CreateDefinition(identifier, nook, (b => f(next(b))))
          case ShapeOf(expr, next) => ShapeOf(expr, ncell => f(next(ncell)))
          case ExamineLocalScope(next) => ExamineLocalScope(s => f(next(s)))
          case ExamineModuleScope(next) => ExamineModuleScope(s => f(next(s)))
          case ExamineState(next) => ExamineState(s => f(next(s)))
          case CreateWorksheet(seed, next) => CreateWorksheet(seed, w => f(next(w)))
          case CreateWorksheetWithExpression(expr, next) => CreateWorksheetWithExpression(expr, w => f(next(w)))
          case SelectCellAsBase(handle, addr, next) => SelectCellAsBase(handle, addr, f(next))
          case SelectTo(handle, addr, next) => SelectTo(handle, addr, f(next))
          case SelectRay(handle, ray, next) => SelectRay(handle, ray, f(next))
          case TargetAddress(handle, addr, next) => TargetAddress(handle, addr, tgtAddr => f(next(tgtAddr)))
          case Extrude(handle, next) => Extrude(handle, addrPair => f(next(addrPair)))
          case Drop(handle, next) => Drop(handle, addrPair => f(next(addrPair)))
          case Paste(handle, addr, expr, next) => Paste(handle, addr, expr, f(next))
          case ExtractCell(handle, addr, next) => ExtractCell(handle, addr, ncell => f(next(ncell)))
        }

    }


  type FreeM[A] = FreeMonad[Statement, A]
  import FreeMonad._

  def liftFree[F[+_], A](fa : F[A])(implicit ev : Functor[F]) : FreeMonad[F, A] =
    Free(ev.map(fa)(Return[F, A](_)))

  def beginModule(moduleName : String) : FreeM[Unit] = 
    liftFree(BeginModule(moduleName, ()))

  def endModule : FreeM[Unit] = 
    liftFree(EndModule(()))

  def parameter(identifier : Identifier, shell : NCell[FrameworkEntry], isThin : Boolean) : FreeM[Parameter] =
    liftFree(CreateParameter(identifier, shell, isThin, identity))

  def definition(identifier : Identifier, nook : NCell[FrameworkEntry]) : FreeM[Definition] =
    liftFree(CreateDefinition(identifier, nook, identity))

  def shapeOf(expr : Expression) : FreeM[NCell[Expression]] = 
    liftFree(ShapeOf(expr, identity))

  def emptyWorksheet : FreeM[WorksheetHandle] = 
    liftFree(CreateWorksheet(Object(Empty), identity))

  def worksheetWithExpression(expr : Expression) : FreeM[WorksheetHandle] = 
    liftFree(CreateWorksheetWithExpression(expr, identity))

  def selectAsBase(handle : WorksheetHandle, addr : CellAddress) : FreeM[Unit] = 
    liftFree(SelectCellAsBase(handle, addr, ()))

  def selectTo(handle : WorksheetHandle, addr : CellAddress) : FreeM[Unit] = 
    liftFree(SelectTo(handle, addr, ()))

  def selectRay(handle : WorksheetHandle, ray : Vector[Int]) : FreeM[Unit] =
    liftFree(SelectRay(handle, ray, ()))

  def targetAddress(handle : WorksheetHandle, addr : CellAddress) : FreeM[CellAddress] = 
    liftFree(TargetAddress(handle, addr, identity))

  def extract(handle : WorksheetHandle, addr : CellAddress) : FreeM[NCell[FrameworkEntry]] =
    liftFree(ExtractCell(handle, addr, identity))

  def extrude(handle : WorksheetHandle) : FreeM[(CellAddress, CellAddress)] =
    liftFree(Extrude(handle, identity)) 

  def drop(handle : WorksheetHandle) : FreeM[(CellAddress, CellAddress)] = 
    liftFree(Drop(handle, identity)) 

  def paste(handle : WorksheetHandle, addr : CellAddress, expr : Expression) : FreeM[Unit] = 
    liftFree(Paste(handle, addr, expr, ()))

  // Debug and state examination
  def examineLocalScope : FreeM[Scope] = 
    liftFree(ExamineLocalScope(identity))

  def examineModuleScope : FreeM[Scope] = 
    liftFree(ExamineModuleScope(identity))

  def examineState : FreeM[ModuleZipper] = 
    liftFree(ExamineState(identity))

}
