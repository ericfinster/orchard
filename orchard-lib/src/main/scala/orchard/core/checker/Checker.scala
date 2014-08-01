/**
  * Checker.scala - The main checker class for Orchard
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.checker

import scalaz._

import orchard.core.util._
import orchard.core.cell._

import ErrorM._
import ErrorMonad._

trait Checker extends CheckerModuleSystem with CheckerFrameworks {

  type CheckerState = (ModuleZipper, Int)
  type CheckerM[+A] = StateT[Error, CheckerState, A]
  type CheckerS[S, +A] = StateT[Error, S , A]

  val M = MonadState[CheckerS, CheckerState]
  import M._

  def getZipper : CheckerM[ModuleZipper] = 
    for {
      st <- get
    } yield st._1

  def getOffset : CheckerM[Int] = 
    for {
      st <- get
    } yield st._2

  def enclosingEnvironment : CheckerM[Vector[ModuleEntry]] =
    for {
      zipper <- getZipper
    } yield {
      zipper.collectLefts
    }

  def localEnvironment : CheckerM[Vector[ModuleEntry]] =
    for {
      zipper <- getZipper
      offset <- getOffset
      _ <- liftError(ensure(zipper.focus.isInstanceOf[Module], "Focus is not a module."))
    } yield {
      zipper.focus.asInstanceOf[Module].entries.take(offset)
    }

  def environment : CheckerM[Vector[ModuleEntry]] =
    for {
      encEnv <- enclosingEnvironment
      localEnv <- localEnvironment
    } yield (encEnv ++ localEnv)

  def environmentExpressions : CheckerM[Vector[CheckerExpressionNode]] = 
    for {
      env <- environment
    } yield {
      (env map {
        case p : Parameter => Some(p.node)
        case d : Definition => Some(d.node)
        case _ => None
      }).flatten
    }

  def environmentContainers : CheckerM[Vector[CheckerContainerNode]] = 
    for {
      env <- environment
    } yield {
      (env map {
        case m : Module => Some(m.node)
        case i : Import => Some(i.node)
        case _ => None
      }).flatten
    }

  def modules : CheckerM[Vector[Module]] = 
    for {
      env <- environment
    } yield {
      (env map {
        case m : Module => Some(m)
        case _ => None
      }).flatten
    }

  def parameters : CheckerM[Vector[Parameter]] = 
    for {
      env <- environment
    } yield {
      (env map {
        case p : Parameter => Some(p)
        case _ => None
      }).flatten
    }

  def parameterQualNames : CheckerM[Vector[String]] = 
    for {
      params <- parameters
    } yield {
      params map (_.node.qualifiedName)
    }

  def identifiers : CheckerM[Vector[String]] = 
    for {
      env <- environment
    } yield {
      (env map {
        case p : Parameter => Vector(p.node.name)
        case d : Definition => Vector(d.node.name, d.node.fillerExpression.name)
        case _ => Vector.empty
      }).flatten
    }

  def environmentTree : CheckerM[RoseTree[String, String]] =
    for {
      env <- environment
    } yield {

      val brs = env map {
        case m : Module => None
        case p : Parameter => Some(Rose(p.node.name))
        case d : Definition => {
          // Here is where you can make entries for the filler and the boundary ...
          Some(Branch(d.node.name, Vector(Rose(d.node.fillerExpression.name))))
        }
        case i : Import => {
          // This is the interesting case ...
          Some(Rose(i.node.name))
        }
      }

      Branch("root", brs.flatten)
    }

  def insertModule(moduleId : String) : CheckerM[Module] =
    for {
      zipper <- getZipper
      offset <- getOffset
      mods <- modules
      modIds = mods map (_.node.name)
      _ <- liftError(ensureNot(modIds contains moduleId, "Module name already exists."))
      moduleNode = new CheckerModuleNode(moduleId)
      module = Module(moduleNode, Vector.empty)
      res <- liftError(zipper.insertAt(module, offset))
      _ <- put(res, 0)
    } yield module

  // Check that the new name doesn't clash ...
  def insertImport(name : String, moduleName : String, isOpen : Boolean) : CheckerM[Import] =
    for {
      zipper <- getZipper
      offset <- getOffset
      mods <- modules
      module <- liftError(
        fromOption(
          mods find (_.node.name == moduleName),
          "Module " ++ moduleName ++ " not found."
        )
      )
      importNode = new CheckerImportNode(name, module, isOpen)
      imprt = Import(importNode, Vector.empty)
      res <- liftError(zipper.insertAt(imprt, offset))
      _ <- put(res, offset + 1)
    } yield imprt


  def insertParameter(identString : String, ncell : NCell[Option[Expression]], isThin : Boolean) : CheckerM[Parameter] =
    for {
      zipper <- getZipper
      offset <- getOffset
      rawIdent <- parseIdentifierString(identString)
      ident <- resolveRawIdentifier(rawIdent)
      envIdents <- identifiers
      _ <- liftError(ensureNot(envIdents contains (ident.expand), "Parameter name already exists."))
      parameterNode = new CheckerParameterNode(ident, new Shell(new CheckerFramework(ncell)), isThin)
      parameter = Parameter(parameterNode)
      res <- liftError(zipper.insertAt(parameter, offset))
      _ <- put(res, offset + 1)
    } yield parameter


  def insertDefinition(identString : String, ncell : NCell[Option[Expression]]) : CheckerM[Definition] = 
    for {
      zipper <- getZipper
      offset <- getOffset
      rawIdent <- parseIdentifierString(identString)
      ident <- resolveRawIdentifier(rawIdent)
      envIdents <- identifiers
      _ <- liftError(ensureNot(envIdents contains (ident.expand), "Filler name already exists."))
      definitionNode = new CheckerDefinitionNode(ident, new Nook(new CheckerFramework(ncell)))
      definition = Definition(definitionNode)
      res <- liftError(zipper.insertAt(definition, offset))
      _ <- put(res, offset + 1)
    } yield definition

  // This is quite obviously wrong.  But we'll refine it later
  def convertible(e0 : Expression, e1 : Expression) : CheckerM[Boolean] = 
    liftError(success(e0 == e1))

  def ensureConvertible(e0 : Expression, e1 : Expression) : CheckerM[Unit] = 
    for {
      areConvertible <- convertible(e0, e1)
      _ <- liftError(
        ensure(areConvertible, "Match error: expression " ++ e0.name ++ " is not convertible to " ++ e1.name)
      )
    } yield ()

  def validatePaste(identifier : String, framework : NCell[Either[CellAddress, Expression]]) : CheckerM[NCell[Expression]] = {

    // Hmm. I think this should be done with a state transformer on top
    // of the checker.  But anyway I think there is some architechture to
    // rethink here.

    import scala.collection.mutable.HashMap

    val bindings : HashMap[CellAddress, Expression] = HashMap.empty

    for {
      exprNCell <- getExpressionNCell(identifier)
      zippedExpr <- liftError(fromOption(framework.zip(exprNCell), "Cells have incompatible shape."))
      requirements = zippedExpr map {
        case (Left(addr), e) => {
          if (bindings.isDefinedAt(addr)) {
            ensureConvertible(bindings(addr), e)
          } else {
            bindings(addr) = e
            point(())
          }
        }
        case (Right(expr), e) => ensureConvertible(expr, e)
      }
      _ <- NCell.sequence[Unit, CheckerM](requirements)
    } yield exprNCell

  }

  def getExpressionNCell(identifier : String) : CheckerM[NCell[Expression]] =
    for {
      exprNodes <- environmentExpressions
      matches = (exprNodes map {
        case p : CheckerParameterNode => 
          if (p.name == identifier) Some(p.variableExpression) else None
        case d : CheckerDefinitionNode =>
          if (d.fillerExpression.name == identifier)
            Some(d.fillerExpression)
          else if (d.boundaryExpression.name == identifier)
            Some(d.boundaryExpression)
          else 
            None
      }).flatten
      _ <- liftError(
        ensure(matches.length > 0, "Identifier " ++ identifier ++ " not found in environment.")
      )
      _ <- liftError(
        ensure(matches.length < 2, "Internal error: multiple expressions match identifier " ++ identifier)
      )
    } yield matches.head.ncell

  def getModule(moduleId : String) : CheckerM[Module] = 
    for {
      mods <- modules
      m <- liftError(
        fromOption(
          mods find (_.node.name == moduleId),
          "Module " ++ moduleId ++ " not found."
        )
      )
    } yield m

  def resolveRawIdentifier(rawIdent : RawIdentifier) : CheckerM[Identifier] = {

    val resolveList = 
      rawIdent.tokens map {
        case RawLiteralToken(lit) => liftError(success(LiteralToken(lit)))
        case RawReferenceToken(ref) => 
          for {
            params <- parameters
            param <- liftError(fromOption(
              params find (p => p.node.name == ref),
              "Failed to resolve identifier reference: " ++ ref
            ))
          } yield ReferenceToken(param)
      }

    import scalaz.std.list._

    for {
      newToks <- sequence(resolveList)
    } yield Identifier(newToks)

  }

  def parseIdentifierString(identString : String) : CheckerM[RawIdentifier] =
    IdentParser(identString) match {
      case IdentParser.Success(rawIdent, _) => liftError(success(rawIdent))
      case _ => liftError(fail("Failed to parse identifier string: " ++ identString))
    }

  def liftError[A](e : Error[A]) : CheckerM[A] =
    StateT[Error, CheckerState, A]((st : CheckerState) => { e map (a => (st, a)) })

}

object ErrorMonad {

  implicit val errorIsMonad : Monad[Error] =
    new Monad[Error] {
      def point[A](a : => A) : Error[A] = success(a)
      def bind[A, B](fa : Error[A])(f : A => Error[B]) : Error[B] = 
        fa match {
          case Right(a) => f(a)
          case Left(msg) => fail(msg)
        }
    }

}
