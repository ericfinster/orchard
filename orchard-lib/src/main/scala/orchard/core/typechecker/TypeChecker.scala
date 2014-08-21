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
            env <- ask
            cellExpr <- resolveCellExpression(ref)
          } yield {
            ReferenceToken(
              env.identifierMap(cellExpr.qualifiedName.toString)
            )
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

  def lookup(key : EnvironmentKey) : Checker[CellExpression] =
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


  def resolveNCell(ncell : NCell[Option[String]]) : Checker[NCell[FrameworkEntry]] =  {

    val resolutions = ncell map {
      case None => succeed(Empty)
      case Some(ref) =>
        for {
          expr <- resolveCellExpression(ref)
        } yield Full(expr) 
        // Here you use the expression itself.  But for many purposes, you want
        // the not the actual cell, but a reference to it.  Just a flag on this method?

    }

    for {
      // Resolve all of the references ...
      resolvedNCell <- NCell.sequence[FrameworkEntry, Checker](resolutions)
    } yield resolvedNCell

  }

  def resolve(cell : CellExpression) : Checker[ConcreteCellExpression] = 
    cell match {
      case c : ConcreteCellExpression => point(c)
      case r : Reference => 
        for {
          next <- lookup(r.index)
          result <- resolve(next)
        } yield result
    }

  def convertible(e : CellExpression, f : CellExpression) : Checker[Boolean] = 
    (e, f) match {
      // For two references, check if they reference the same thing. If
      // not, unfold them and recheck
      case (u : Reference, v : Reference) =>
        if (u == v) point(true) else
          for {
            g <- resolve(u)
            h <- resolve(v)
            converts <- convertible(g, h)
          } yield converts

      // If we have a reference and a concrete cell, resolve the reference ...
      case (u : Reference, _) =>
        for {
          g <- resolve(u)
          converts <- convertible(g, f)
        } yield converts

      // and dually.
      case (_, v : Reference) =>
        for {
          h <- resolve(v)
          converts <- convertible(e, h)
        } yield converts

      // Compare two variables
      case (u : Variable, v : Variable) => 
        if (
          (u.qualifiedName.toString == v.qualifiedName.toString) &&
            (u.isThin == v.isThin)
        ) {
          for {
            shellsConvert <- ncellConvertible(
              u.shell.ncell,
              v.shell.ncell
            )
          } yield shellsConvert
        } else point(false)

      // Compare to fillers
      case (u : Filler, v : Filler) => 
        for {
          nooksConvert <- ncellConvertible(
            u.nook.ncell,
            v.nook.ncell
          )
        } yield nooksConvert

      // Compare two boundaries (just compare their interiors)
      case (u : Filler#BoundaryExpr, v : Filler#BoundaryExpr) =>
        for {
          interiorsConvert <- convertible(u.interior, v.interior)
        } yield interiorsConvert

      case _ => point(false)
    }
    
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
          booleanCell <- NCell.sequence(constraints)
        } yield booleanCell forall identity

      }
    }

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

          mods <- modulesInScope
          _ <- attempt(
            ensure(  // Er... this doesn't look quite right ...
              ! (mods exists (_.qualifiedName.localName == name)),
              "Module name " ++ name ++ " already exists"
            )
          )

          qualPrefix = env.qualifiedPrefix :+ name
          moduleEnv = env.copy(qualifiedPrefix = qualPrefix)

          checkedContents <- scope(moduleEnv)(check(contents))

        } yield {
          Vector(
            new ModuleExpression(
              QualifiedName(env.qualifiedPrefix, name),
              checkedContents,
              env.expressions.length
            )
          )
        }


      case Parameter(identString, shell, isThin) => 
        for {
          env <- ask

          ident <- resolveIdentString(identString)
          name <- ident.expand
          qn = QualifiedName(env.qualifiedPrefix, name)

          resolvedShell <- resolveNCell(shell)

          completeShell <- attempt(
            Shell(resolvedShell)
          )

          // Here we do absolutely nothing to verify that the resolved shell
          // is well typed in the current environment.  This means you could
          // easily cheat the checker to come up with malformed cells by
          // using the syntax directly ....

        } yield {

          println("Added parameter: " ++ qn.toString)

          Vector(
            Variable(qn, ident, completeShell, isThin)
          )
        }

      case Definition(identString, nook) =>
        for {
          env <- ask

          ident <- resolveIdentString(identString)
          name <- ident.expand
          qn = QualifiedName(env.qualifiedPrefix, name)

          resolvedNook <- resolveNCell(nook)

          completeNook <- attempt(
            Nook(resolvedNook)
          )

        } yield {

          println("Added definition: " ++ qn.toString)

          val filler = Filler(qn, ident, completeNook)
          Vector(filler.Boundary, filler)

        }

      case Import(name, moduleName, shell, bindings) => 
        for {
          env <- ask

          // Locate the requested module
          module <- resolveModuleExpression(moduleName)

          // Resolve the shell in which the import will be applied
          resolvedShell <- resolveNCell(shell)
          completeShell <- attempt(
            Shell(resolvedShell)
          )

        } yield {

          // Now what?  We have these bindings.  What we need to do is check that they
          // are compatible and create the new shapes.  How does this work?

          // Right now, of course, you have not really exploited this whole reference system,
          // since when you resolve an n-cell, you actually pick out the typed expressions and
          // not their references.  What this means is that if you proceed by rewriting all
          // of the faces recursively, you are going to rewrite the same cell many times over.

          // This is exactly what you want to avoid.  This is what the whole reference system
          // is *for*.

          // So, roughly, what you are going to do is the following:
          //
          //   1)  Pass over the bindings.  Check the compatibility of the bound cell with the
          //       binding.  This may induce some sub-bindings which you will keep in a list
          //       of compatibilities left to check
          //   
          //   2)  When this pass is complete, you will pass over the cells in the module itself,
          //       promoting any unbound variables to new parameters and modifying the definition
          //       cells appropriately as well.
          // 

          //  How do I check the compatibility of a binding?  Right, well, you simply grab the ncells
          //  of the referenced entities and zip them.  This checks that they have compatible shape.
          //  Next you check that these expressions match point wise except for where the target has
          //  variables, and these become requirements for the bind to complete.

          //  Okay, I think we are going to need a transformed monad to handle the bindings which
          //  includes the current binding state.  This shouldn't be too bad.

          Vector.empty

        }

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
