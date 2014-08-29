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

import scalaz.syntax.traverse._

import orchard.core.util._
import orchard.core.cell._
import ErrorM._
import MonadUtils._

trait TypeChecker 
    extends Bindings
    with Frameworks
    with Identifiers 
    with TypedExpressions {

  type EnvironmentKey = Int

  type Checker[+A] = Kleisli[Error, Environment, A]
  type CheckerT[M[+_], A] = Kleisli[M, Environment, A]
  type CheckerE[E, A] = Kleisli[Error, E, A]

  object CheckerErrorSyntax extends ErrorLifts[CheckerT]
  import CheckerErrorSyntax._

  val CheckerMR = MonadReader[CheckerE, Environment]
  import CheckerMR._

  //============================================================================================
  // IDENTIFIERS AND QUALIFIED NAMES
  //

  def qualify(name : String) : Checker[QualifiedName] = 
    for {
      env <- ask
    } yield QualifiedName(env.qualifiedPrefix, name)

  def parseQualifiedName(str : String) : Checker[QualifiedName] = {

    val components = str.split('.').toVector

    if (components.length <= 0) {
      fail("Qualified name was empty")
    } else {
      succeed(QualifiedName(components.init, components.last))
    }

  }

  def parseIdentString(identString : String) : Checker[RawIdentifier] =
    IdentParser(identString) match {
      case IdentParser.Success(rawIdent, _) => succeed(rawIdent)
      case _ : IdentParser.NoSuccess => 
        fail("Could not parse identifier string: " ++ identString)
    }

  def resolveRawIdent(rawIdent : RawIdentifier) : Checker[Identifier] = {

    val resolutionList : List[Checker[IdentifierToken]] = 
      rawIdent.tokens map {
        case RawLiteralToken(lit) => succeed(LiteralToken(lit))
        case RawReferenceToken(ref) => 
          for {
            reference <- resolveNameAsReference(ref)
          } yield {
            ReferenceToken(reference)
          }
      }

    import scalaz.std.list._

    for {
      newIdents <- sequence(resolutionList)
    } yield Identifier(newIdents)

  }

  def resolveIdentString(identString : String) : Checker[Identifier] = 
    for {
      rawIdent <- parseIdentString(identString)
      ident <- resolveRawIdent(rawIdent)
    } yield ident

  //============================================================================================
  // ENVIRONMENT RESOLUTION
  //

  def resolveName(name : String) : Checker[TypedExpression] =
    for {
      env <- ask
      qn <- parseQualifiedName(name)

      // We start in the current scope and work up ...
      prefixes = env.qualifiedPrefix.inits.toVector
      testList = prefixes map (qn.withPrefix(_).toString)

      matchedName <- attempt(
        fromOption(
          testList find (env.identifierMap.isDefinedAt(_)),
          "Name resolution failed for: " ++ name
        )
      )

    } yield {
      env.expressions(env.identifierMap(matchedName))
    }

  def resolveCellExpression(name : String) : Checker[CellExpression] = 
    for {
      expr <- resolveName(name)

      _ <- attempt(
        ensure(
          expr.isInstanceOf[CellExpression],
          "Resolved name " ++ name ++ " was expected to reference a cell"
        )
      )

    } yield expr.asInstanceOf[CellExpression]

  def resolveModuleExpression(name : String) : Checker[ModuleExpression] = 
    for {
      expr <- resolveName(name)

      _ <- attempt(
        ensure(
          expr.isInstanceOf[ModuleExpression],
          "Resolved name " ++ name ++ " was expected to reference a module"
        )
      )

    } yield expr.asInstanceOf[ModuleExpression]


  def resolveNameAsReference(name : String) : Checker[Reference] = 
    for {
      cellExpr <- resolveCellExpression(name)
    } yield Reference(cellExpr)


  def resolveFramework(ncell : NCell[Option[String]]) : Checker[NCell[ExprReference]] =
    for {

      env <- ask

      resolutions = (ncell map {
        case None => succeed(EmptyReference)
        case Some(ref) => resolveNameAsReference(ref)
      }) : NCell[Checker[ExprReference]]

      framework <- resolutions.sequence //NCell.sequence(resolutions)

    } yield framework

  def resolveKey(key : EnvironmentKey) : Checker[CellExpression] =
    for {
      env <- ask

      _ <- attempt(
        ensure(
          env.expressions.isDefinedAt(key),
          "Environment index out of range: " ++ key.toString
        )
      )

      entry = env.expressions(key)

      _ <- attempt(
        ensure(
          entry.isInstanceOf[CellExpression],
          "Internal error: cell references a module"
        )
      )

    } yield entry.asInstanceOf[CellExpression]

  //============================================================================================
  // CONVERTIBILITY
  //

  def convertible(e : CellExpression, f : CellExpression) : Checker[Boolean] = ???
    // (e, f) match {
    //   // For two references, check if they reference the same thing. If
    //   // not, unfold them and recheck
    //   case (u : Reference, v : Reference) =>
    //     if (u == v) point(true) else
    //       for {
    //         g <- resolve(u)
    //         h <- resolve(v)
    //         converts <- convertible(g, h)
    //       } yield converts

    //   // If we have a reference and a concrete cell, resolve the reference ...
    //   case (u : Reference, _) =>
    //     for {
    //       g <- resolve(u)
    //       converts <- convertible(g, f)
    //     } yield converts

    //   // and dually.
    //   case (_, v : Reference) =>
    //     for {
    //       h <- resolve(v)
    //       converts <- convertible(e, h)
    //     } yield converts

    //   // Compare two variables
    //   case (u : Variable, v : Variable) => 
    //     if (
    //       (u.qualifiedName.toString == v.qualifiedName.toString) &&
    //         (u.isThin == v.isThin)
    //     ) {
    //       for {
    //         shellsConvert <- ncellConvertible(
    //           u.shell.ncell,
    //           v.shell.ncell
    //         )
    //       } yield shellsConvert
    //     } else point(false)

    //   // Compare two fillers
    //   case (u : Filler, v : Filler) => 
    //     for {
    //       nooksConvert <- ncellConvertible(
    //         u.nook.ncell,
    //         v.nook.ncell
    //       )
    //     } yield nooksConvert

    //   // Compare two boundaries (just compare their interiors)
    //   case (u : Filler#BoundaryExpr, v : Filler#BoundaryExpr) =>
    //     for {
    //       interiorsConvert <- convertible(u.interior, v.interior)
    //     } yield interiorsConvert

    //   case _ => point(false)
    // }
    
  def ncellConvertible(e : NCell[FrameworkEntry], f : NCell[FrameworkEntry]) : Checker[Boolean] =
    e.zip(f) match {
      case None => point(false)
      case Some(matchedShape) => {

        val constraints =
          matchedShape map {
            case (Full(a), Full(b)) => convertible(a, b)
            case (Empty, Empty) => point(true)
            case _ => point(false)
          }

        for {
          booleanCell <- constraints.sequence // NCell.sequence(constraints)
        } yield booleanCell forall identity

      }
    }

  def ensureConvertible(e : CellExpression, f : CellExpression) : Checker[Unit] = 
    for {
      areConvertible <- convertible(e, f)

      _ <- attempt(
        ensure(
          areConvertible,
          "Expressions " ++ e.toString ++ " and " ++ f.toString ++ " are not convertible"
        )
      )
    } yield ()

  //============================================================================================
  // TYPE CHECKING
  //

  def check(exprs : Vector[Expression]) : Checker[Vector[TypedExpression]] =
    if (exprs.length <= 0) {
      point(Vector.empty)
    } else {
      for {
        env <- ask
        headExprs <- check(exprs.head)
        tailExprs <- scope(env.extendWith(headExprs))(check(exprs.tail))
      } yield headExprs ++ tailExprs
    }

  def check(expr : Expression) : Checker[Vector[TypedExpression]] =
    expr match {

      case Module(name, contents) => 
        for {
          env <- ask

          moduleQn <- qualify(name)
          moduleQnStr = moduleQn.toString

          mods <- modulesInScope
          _ <- attempt(
            ensure(  
              ! (mods exists (_.qualifiedName.toString == moduleQnStr)),
              "Module name " ++ name ++ " already exists"
            )
          )

          qualPrefix = env.qualifiedPrefix :+ name
          moduleEnv = env.copy(qualifiedPrefix = qualPrefix)

          checkedContents <- scope(moduleEnv)(check(contents))

        } yield {
          Vector(
            new ModuleExpression(
              env.expressions.length,
              moduleQn,
              moduleEnv.extendWith(checkedContents)
            )
          )
        }


      case Parameter(identString, framework, isThin) => 
        for {
          env <- ask

          ident <- resolveIdentString(identString)
          qn = QualifiedName(env.qualifiedPrefix, ident.expand)

          resolvedFramework <- resolveFramework(framework)

          shell <- attempt(
            Shell(resolvedFramework) 
          )

          // Here we do absolutely nothing to verify that the resolved shell
          // is well typed in the current environment.  This means you could
          // easily cheat the checker to come up with malformed cells by
          // using the syntax directly ....

        } yield {

          println("Added parameter: " ++ qn.toString)

          Vector(
            new Variable(env.expressions.length, qn, ident, shell, isThin)
          )
        }

      case Definition(identString, framework) =>
        for {
          env <- ask

          ident <- resolveIdentString(identString)
          qn = QualifiedName(env.qualifiedPrefix, ident.expand)

          resolvedFramework <- resolveFramework(framework)

          nook <- attempt(
            Nook(resolvedFramework)
          )

        } yield {

          println("Added definition: " ++ qn.toString)

          val filler = new Filler(env.expressions.length, qn, ident, nook)
          Vector(filler.Boundary, filler)

        }

      case Import(name, moduleName, framework, bindings) => 
        for {
          env <- ask

          // Locate the requested module
          module <- resolveModuleExpression(moduleName)
          moduleEnv = module.moduleEnvironment

          // Resolve the shell in which the import will be applied

          resolvedFramework <- resolveFramework(framework)
          shell <- attempt(
            Shell(resolvedFramework)
          )

          // Let's try to make a list of the expression we want to 
          // bind.

          _ = {

            val bindingList = bindings map {
              case (varName, exprName) =>
                for {
                  varExpr <- resolveCellExpression(varName)

                  _ <- attempt(
                    ensure(
                      varExpr.isInstanceOf[Variable],
                      "Cannot bind to non-variable " ++ varName
                    )
                  )

                } yield {
                }
            }

          }

        } yield {

          Vector.empty

        }

    }

  //============================================================================================
  // UTILITIES
  //

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

  def modulesInScope : Checker[Vector[ModuleExpression]] = 
    for {
      exprs <- expressions
    } yield {
      (exprs map {
        case m : ModuleExpression => Some(m)
        case _ => None
      }).flatten
    }

  def doCheck(expr : Expression) : Unit = 
    check(expr)(emptyEnvironment) match {
      case Right(exprs) => {
        println("\nChecking succeeded!\n")
        println(TypedExpression.prettyPrint(exprs)(""))
      }
      case Left(msg) => {
        println("Error: " ++ msg)
      }
    }

  def dumpEnv : Checker[Unit] = 
    for {
      env <- ask
    } yield println(env.toString)

  //============================================================================================
  // RAW SYNTAX
  //

  sealed trait Expression
  case class Module(val name : String, val contents : Vector[Expression]) extends Expression
  case class Parameter(val identString : String, val shell : NCell[Option[String]], val isThin : Boolean) extends Expression
  case class Definition(val identString : String, val nook : NCell[Option[String]]) extends Expression
  case class Import(val name : String, val moduleName : String, val shell : NCell[Option[String]], val bindings : Map[String, String]) extends Expression

  //============================================================================================
  // ENVIRONMENT DEFINITION
  //

  case class Environment(
    val expressions : Vector[TypedExpression],
    val identifierMap : Map[String, Int],
    val qualifiedPrefix : Vector[String]
  ) {

    def extendWith(exprs : Vector[TypedExpression]) = {

      val length = expressions.length

      val newExpressions = expressions ++ exprs
      val newIdentifierMap = identifierMap ++
        (exprs.zipWithIndex map {
          case (e, i) => (e.qualifiedName.toString, i + length)
        }).toMap

      this.copy(
        expressions = newExpressions,
        identifierMap = newIdentifierMap
      )

    }

    override def toString = {
      "\n-----------------------\n" ++
      "Environment " ++ qualifiedPrefix.mkString("(", ".", ")\n\n") ++
      "Expressions:\n" ++ (expressions map (_.toString)).mkString("", "\n", "\n") ++
      "\nIdent Map:\n" ++ (identifierMap.toVector map { case (s, i) => s ++ " -> " ++ i.toString }).mkString("", "\n", "\n") ++
      "-----------------------\n"

    }

  }

  def emptyEnvironment : Environment = 
    Environment(Vector.empty, Map.empty, Vector.empty)

}

object SimpleTypeChecker extends TypeChecker
