/**
  * CheckerSyntax.scala - Syntax module
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.typechecker

trait CheckerSyntax { thisChecker : Checker => 

  sealed trait Statement
  case class ModuleStatement(val name : String, val entries : Vector[Statement]) extends Statement
  case class ImportStatment(val name : String, val moduleName : String) extends Statment
  case class ParameterStatement(val identifier : RawIdentifier, val shell : NCell[Option[String]]) extends Statement
  case class DefinitionStatement(val identifier : RawIdentifier, val nook : NCell[Option[String]]) extends Statement

  // Okay, right.  Remember that the idea was to make a free monad out of this guy.
  // Good, yes.  Let's do it this way.  Then you have two things you can do with this.

  // One is that you can implement a zipper which allows you to modify the syntax tree in place.
  // The second is that you can implement and interpreter which progressively builds an environment
  // in which the statements can be interpreted.

}

// import scalaz._

// sealed trait Statement[+A]
// case class ParameterDec[+A](name : String, next : A) extends Statement[A]
// case class LiftDec[+A](name : String, next : A) extends Statement[A]
// case object Done extends Statement[Nothing]

// object Statement {

//   implicit def isFunctor : Functor[Statement] = 
//     new Functor[Statement] {

//       def map[A, B](fa : Statement[A])(f : A => B) : Statement[B] = 
//         fa match {
//           case ParameterDec(name, next) => ParameterDec(name, f(next))
//           case LiftDec(name, next) => LiftDec(name, f(next))
//           case Done => Done
//         }

//     }

//   // Now, I want to use the free monad.

//   type FreeM[A] = Free[Statement, A]
//   // val M = Monad[FreeM]
//   import Free._

//   def paramDec(name : String) : FreeM[Unit] = 
//     Suspend(ParameterDec(name, Return(())))

//   def liftDec(name : String) : FreeM[Unit] = 
//     Suspend(LiftDec(name, Return(())))

//   def test : FreeM[Unit] = 
//     for {
//       _ <- paramDec("Eric")
//       _ <- liftDec("Fred")
//     } yield ()

// }
