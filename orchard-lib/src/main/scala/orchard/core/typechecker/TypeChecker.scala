/**
  * TypeChecker.scala - Another go at a type checker
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.typechecker

import scala.language.higherKinds

import scalaz._
import Kleisli._

import orchard.core.util._
import orchard.core.cell._
import ErrorM._
import MonadUtils._

trait TypeChecker 
    extends TypedExpressions
    with Frameworks
    with Identifiers {

  // Right, well, I'm not sure what the final environment will
  // end up being.  But it seems that we need at least some idea
  // of the current module structure so that we can we can use
  // qualified naming ....

  case class Environment(
    val expressions : Vector[TypedExpression],
    val identifierMap : Map[String, Int],
    val qualifiedPrefix : Vector[String]
  )

  // type Environment = Vector[TypedExpression]
  type EnvironmentKey = Int

  type Checker[+A] = Kleisli[Error, Environment, A]
  type CheckerT[M[+_], A] = Kleisli[M, Environment, A]
  type CheckerE[E, A] = Kleisli[Error, E, A]

  object CheckerErrorSyntax extends ErrorLifts[CheckerT]
  import CheckerErrorSyntax._

  val CheckerReader = MonadReader[CheckerE, Environment]
  import CheckerReader._

  def expressions : Checker[Vector[TypedExpression]] =
    for {
      env <- ask
    } yield env.expressions

  def identifierMap : Checker[Map[String, Int]] = 
    for {
      env <- ask
    } yield env.identifierMap

  def qualifiedPrefix : Checker[Vector[String]] = 
    for {
      env <- ask
    } yield env.qualifiedPrefix

  def lookup(key : EnvironmentKey) : Checker[CellExpression] = ???
    // for {
    //   exprVector <- expressionVector
    //   envLength = exprVector.length
    //   _ <- attempt(
    //     ensure(
    //       (key >= 0) && (key < envLength),
    //       "Environment key out of range: " ++ key.toString
    //     )
    //   )
    //   entry = env(envLength - key - 1) 
    //   _ <- attempt(
    //     ensure(
    //       entry.isInstanceOf[CellExpression],
    //       "Internal error: cell references a module"
    //     )
    //   )
    // } yield entry.asInstanceOf[CellExpression]

  def parseIdentString(identString : String) : Checker[RawIdentifier] =
    IdentParser(identString) match {
      case IdentParser.Success(rawIdent, _) => succeed(rawIdent)
      case _ : IdentParser.NoSuccess => 
        fail("Could not parse identifier string: " ++ identString)
    }

  // def resolveRawIdent(rawIdent : RawIdentifier) : Checker[Identifier] = {

  // }

  //   def processRawIdentifier(scope : Scope, rawIdent : RawIdentifier) : CheckerResult[Identifier] = {
  //     val idents : List[CheckerResult[IdentifierToken]] =
  //       rawIdent.tokens map {
  //         case RawLiteralToken(lit) => CheckerResult(LiteralToken(lit))
  //         case RawReferenceToken(ref) =>
  //           for {
  //             resultRef <- lookupIdentifier(ref, scope)
  //           } yield ReferenceToken(resultRef)
  //       }

  //     for {
  //       newIdents <- sequence(idents)
  //     } yield Identifier(newIdents)
  //   }

  def check(exprs : Vector[Expression]) : Checker[Vector[TypedExpression]] =
    if (exprs.length <= 0) {
      point(Vector.empty)
    } else {
      for {
        env <- ask
        headExprs <- check(exprs.head)
        extendedEnvironment = env.copy(expressions = env.expressions ++ headExprs)
        tailExprs <- scope(extendedEnvironment)(check(exprs.tail))
      } yield headExprs ++ tailExprs
    }

  def check(expr : Expression) : Checker[Vector[TypedExpression]] =
    expr match {

      // Should process the names and stuff as well.
      case Module(name, contents) => {

        // The first thing to do is prepare the module environment.  This means appending to
        // the current qualified setup as well as figuring out where you are going to store
        // the qualified names ....

        // Okay, the idea is that first you make a new map and environment
        // with only references and with the names reset.  Then you check
        // in that new scope ....

        for {
          qualPref <- qualifiedPrefix
          checkedContents <- check(contents)
        } yield {
          Vector(
            new ModuleExpression(
              QualifiedName(qualPref, name),
              checkedContents
            )
          )
        }
      }

      case Parameter(name, shell, isThin) => 
        for {
          rawIdent <- parseIdentString(name)
        } yield ???


      case Definition(name, nook) => ???
      case Import(name, moduleName, bindings) => ???

    }

  //============================================================================================
  // RAW SYNTAX
  //

  sealed trait Expression
  case class Module(val name : String, val contents : Vector[Expression]) extends Expression
  case class Parameter(val name : String, val shell : NCell[Option[String]], val isThin : Boolean) extends Expression
  case class Definition(val name : String, val nook : NCell[Option[String]]) extends Expression
  case class Import(val name : String, val moduleName : String, val bindings : Map[String, String]) extends Expression

}

object SimpleTypeChecker extends TypeChecker
