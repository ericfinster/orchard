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

  }

  //============================================================================================
  // STATEMENTS
  //

  sealed trait Statement[+A]

  case class BeginModule[A](moduleName : String, next : A) extends Statement[A]
  case class EndModule[A](next : A) extends Statement[A]

  case class CreateParameter[A](identifier : Identifier, shell : NCell[FrameworkEntry], isThin : Boolean, next : Parameter => A) extends Statement[A]
  case class CreateDefinition[A](identifier : Identifier, nook : NCell[FrameworkEntry], next : Definition => A) extends Statement[A]

  implicit def statementIsFunctor : Functor[Statement] = 
    new Functor[Statement] {

      def map[A, B](fa : Statement[A])(f : A => B) : Statement[B] = 
        fa match {
          case BeginModule(moduleName, next) => BeginModule(moduleName, f(next))
          case EndModule(next) => EndModule(f(next))
          case CreateParameter(identifier, shell, isThin, next) => CreateParameter(identifier, shell, isThin, (v => f(next(v))))
          case CreateDefinition(identifier, nook, next) => CreateDefinition(identifier, nook, (b => f(next(b))))
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


}
