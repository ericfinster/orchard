/**
  * Checker.scala - The main checker class for Orchard
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.checker

import scalaz._

import orchard.core.util._
import ErrorM._
import ErrorMonad._

trait Checker extends CheckerModuleSystem {

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
          ???
        }
      }

      Branch("root", brs.flatten)
    }

  def insertModule(moduleId : String) : CheckerM[CheckerModuleNode] =
    for {
      zipper <- getZipper
      offset <- getOffset
      mods <- modules
      modIds = mods map (_.node.name)
      _ <- liftError(ensureNot(modIds contains moduleId, "Module name already exists."))
      moduleNode = new CheckerModuleNode(moduleId)
      res <- liftError(zipper.insertAt(Module(moduleNode, Vector.empty), offset))
      _ <- put(res, 0)
    } yield moduleNode


  def insertParameter(identString : String, shell : Shell, isThin : Boolean) : CheckerM[CheckerParameterNode] =
    for {
      zipper <- getZipper
      offset <- getOffset
      rawIdent <- parseIdentifierString(identString)
      ident <- resolveRawIdentifier(rawIdent)
      envIdents <- identifiers
      _ <- liftError(ensureNot(envIdents contains (ident.expand), "Parameter name already exists."))
      parameterNode = new CheckerParameterNode(ident, shell, isThin)
      res <- liftError(zipper.insertAt(Parameter(parameterNode), offset))
      _ <- put(res, offset + 1)
    } yield parameterNode

  def insertDefinition(identString : String, nook : Nook) : CheckerM[CheckerDefinitionNode] = 
    for {
      zipper <- getZipper
      offset <- getOffset
      rawIdent <- parseIdentifierString(identString)
      ident <- resolveRawIdentifier(rawIdent)
      envIdents <- identifiers
      _ <- liftError(ensureNot(envIdents contains (ident.expand), "Filler name already exists."))
      definitionNode = new CheckerDefinitionNode(ident, nook)
      res <- liftError(zipper.insertAt(Definition(definitionNode), offset))
      _ <- put(res, offset + 1)
    } yield definitionNode

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
