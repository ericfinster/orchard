/**
  * CheckerSyntax.scala - Syntax module
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.typechecker

import scalaz._

import orchard.core.cell._
import orchard.core.util._

import ErrorM._
import MonadUtils._

trait CheckerSyntax { thisChecker : Checker => 

  sealed trait Expr
  case class Var(name : String, shell : NCell[Option[Expr]], isThin : Boolean) extends Expr
  case class Bdry(name : String, nook : NCell[Option[Expr]]) extends Expr {
    object Interior extends Expr
  }

  case class WorksheetHandle(index : Int)

  sealed trait Statement[+A]

  case class BeginModuleStatement[A](val name : String, val next : A) extends Statement[A]
  case class EndModuleStatement[A](val name : String, val next : A) extends Statement[A]

  // case class ImportStatement[A](val name : String, val moduleName : String, val bindings : Map[String, String], val next : A) extends Statement[A]

  case class ParameterStatement[A](val identifier : String, val shell : NCell[Option[Expr]], val isThin : Boolean, val next : Var => A) extends Statement[A]
  case class DefinitionStatement[A](val identifier : String, val nook : NCell[Option[Expr]], val next : Bdry => A) extends Statement[A]

  case class NewWorksheetStatement[A](val next : WorksheetHandle => A) extends Statement[A]
  case class SelectCellStatement[A](val handle : WorksheetHandle, val addr : CellAddress, val next : A) extends Statement[A]

  implicit def isFunctor : Functor[Statement] = 
    new Functor[Statement] {

      def map[A, B](fa : Statement[A])(f : A => B) : Statement[B] = 
        fa match {
          case BeginModuleStatement(name, next) => BeginModuleStatement(name, f(next))
          case EndModuleStatement(name, next) => EndModuleStatement(name, f(next))
          // case ImportStatement(name, moduleName, bindings, next) => ImportStatement(name, moduleName, bindings, f(next))
          case ParameterStatement(identifier, shell, isThin, next) => ParameterStatement(identifier, shell, isThin, (v => f(next(v))))
          case DefinitionStatement(identifier, nook, next) => DefinitionStatement(identifier, nook, (b => f(next(b))))
          case NewWorksheetStatement(next) => NewWorksheetStatement(w => f(next(w)))
          case SelectCellStatement(handle, addr, next) => SelectCellStatement(handle, addr, f(next))
        }

    }

  type FreeM[A] = Free[Statement, A]
  import Free._


  def liftFree[F[+_], A](fa : F[A])(implicit ev : Functor[F]) : Free[F, A] = 
    Suspend(ev.map(fa)(Return[F, A](_)))

  // Now, the idea is to implement an interpreter in some kind of environment which produces compiled
  // versions of these guys.

  def beginModule(name : String) : FreeM[Unit] = 
    liftFree(BeginModuleStatement(name, ()))

  def endModule(name : String) : FreeM[Unit] = 
    liftFree(EndModuleStatement(name, ()))

  def parameter(identifier : String, shell : NCell[Option[Expr]], isThin : Boolean) : FreeM[Var] =
    liftFree(ParameterStatement(identifier, shell, isThin, identity))

  def definition(identifier : String, nook : NCell[Option[Expr]]) : FreeM[Bdry] =
    liftFree(DefinitionStatement(identifier, nook, identity))

  def newWorksheet : FreeM[WorksheetHandle] = 
    liftFree(NewWorksheetStatement(identity))

  def selectCell(handle : WorksheetHandle, addr : CellAddress) : FreeM[Unit] = 
    liftFree(SelectCellStatement(handle, addr, ()))

  //============================================================================================
  // EXAMPLES
  //

  def id : FreeM[Bdry] = 
    for {
      x <-   parameter("x", Object(None), false)
      i <-   definition("id-${x}", Object(Some(x)).drop(None, None))
    } yield i

  def comp : FreeM[Bdry] =
    for {
      x <-   parameter("x", Object(None), false)
      y <-   parameter("y", Object(None), false)
      z <-   parameter("z", Object(None), false)
      f <-   parameter("f", Object(Some(x)).glob(Some(y), None), false)
      g <-   parameter("g", Object(Some(y)).glob(Some(z), None), false)
      c <-   definition(
               "(${g} o ${f})", 
               simplex(Some(x), Some(y), Some(z), Some(f), Some(g), None, None)
             )
    } yield c

  def simplex[A](x : A, y : A, z : A, f : A, g : A, h : A, a : A) : NCell[A] = {
    val xPoint = Object(x)
    val fArrow = Composite(f, Seed(xPoint), y)
    val gArrow = Composite(g, fArrow.target.corolla, z)
    Composite(a, Graft(gArrow, Vector(fArrow.corolla)), h)
  }

  def worksheetExample : FreeM[Unit] =
    for {
      _ <- beginModule("Test")
      w <-   newWorksheet
      _ <-   selectCell(w, Immediate)
      _ <- endModule("Test")
    } yield ()

}

