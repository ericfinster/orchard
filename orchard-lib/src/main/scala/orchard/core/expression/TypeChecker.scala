/**
  * TypeChecker.scala - A type checker for Orchard
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

import scala.language.higherKinds

import orchard.core.cell._

abstract class TypeChecker 
    extends SyntaxModule
    with IdentifierModule 
    with ExpressionModule 
    with ModuleModule
    with EnvironmentModule
    with FrameworkModule 
    with SerializationModule {

  type EditorType <: Editor
  def editor : EditorType

  def rootModule : ModuleType

  //============================================================================================
  // BINDING ROUTINES
  //

  def zipShapes[A, B](leftShape : NCell[A], rightShape : NCell[B]) : CheckerResult[NCell[(A, B)]] = 
    leftShape.zip(rightShape) match {
      case None => CheckerFailure("Incompatible shape.")
      case Some(zipped) => CheckerResult(zipped)
    }

  def canPaste(target : NCell[Option[Expression]], expr : NCell[Expression]) : CheckerResult[Boolean] =
    for {
      pairs <- zipShapes(target, expr)
      okToPasteShape <- shapeSequence(
        pairs map {
          case (None, _) => CheckerResult(true)
          case (Some(e0), e1) => 
            if (convertsTo(e0, e1))
              CheckerResult(true)
            else
              CheckerFailure("Expression " ++ e0.name ++ " does not convert to " ++ e1.name)
        }
      )
    } yield true

  def convertsTo(expr1 : Expression, expr2 : Expression) : Boolean = {
    // obviously, this will need to be more complicated in the future
    println("Checking conversion between " ++ expr1.name ++ " and " ++ expr2.name)
    expr1 == expr2
  }

  //============================================================================================
  // RAW SYNTAX COMPILATION
  //

  import scalaz._
  import std.list._

  type Error[+A] = \/[String, A]
  type BuilderState = Map[String, ModuleEntryType]
  type Builder[A] = StateT[Error, BuilderState, A]
  type BuilderM[S, A] = StateT[Error, S , A]

  val M = MonadState[BuilderM, BuilderState]
  import M._

  val T = Traverse[List]

  def builderSuccess[A](a : A) : Error[A] = \/-(a)
  def builderFailure(cause : String) : Error[Nothing] = -\/(cause)

  def insertState[A](st : BuilderState, e : Error[A]) : Error[(BuilderState, A)] = 
    e map (a => (st, a))

  def liftK[A](e : Error[A]) : Builder[A] = 
    StateT(st => insertState(st, e))

  def buildEntries(statement : Statement) : Builder[ModuleEntryType] = 
    statement match {
      case ModuleDefinition(name, body) => 
        for {
          env <- get
        } yield newModule(name)
      case ModuleImport(_, _, _) => ???
      case ParameterDefinition(rawIdent, shell, isThin) =>
        for {
          env <- get
          ident <- resolveRawIdentifier(rawIdent)
        } yield {
          val variable = Variable(ident, ???, isThin)
          newParameter(variable)
        }
      case LiftDefinition(rawBdryIdent, nook) =>
        for {
          env <- get
          ident <- resolveRawIdentifier(rawBdryIdent)
        } yield {
          val filler = Filler(ident, ???)
          newLift(filler)
        }
    }

  def resolveRawIdentifier(rawIdent : RawIdentifier) : Builder[Identifier] = {
    def resolveToken(token : RawIdentifierToken, env : BuilderState) : Error[IdentifierToken] = 
      token match {
        case RawLiteralToken(lit) => builderSuccess(LiteralToken(lit))
        case RawReferenceToken(name) =>
          for { 
            entry <- findEntry(name, env) 
          } yield ReferenceToken(newIdentifierEntry(entry.asInstanceOf[ExpressionEntryType]))
      }

    for {
      env <- get
      tokens <- T.sequence(rawIdent.tokens map (tok => liftK(resolveToken(tok, env))))
    } yield Identifier(tokens)
  }

  def findEntry(name : String, env : Map[String, ModuleEntryType]) : Error[ModuleEntryType] =
    if (env.isDefinedAt(name)) 
      builderSuccess(env(name))
    else
      builderFailure("Unresolved reference: " ++ name)

  //============================================================================================
  // MODULE MANAGEMENT
  //

  def appendSubmodule(module : ModuleType, rawModuleId : String) : CheckerResult[ModuleType] = {
    ModuleIdentParser(rawModuleId) match {
      case ModuleIdentParser.Success(moduleId, _) => {
        if (qualifiedModules(getEnvironment(module)) contains moduleId) {
          CheckerFailure("Submodule " ++ moduleId ++ " already exists.")
        } else {
          val subMod = newModule(moduleId)
          module.appendEntry(subMod)
          CheckerResult(subMod)
        }
      }
      case _ : ModuleIdentParser.NoSuccess => 
        CheckerFailure("Invalid module identifier: " ++ rawModuleId)
    }
  }

  def appendParameter(module : ModuleType, rawVarId : String, shell : Shell, isThin : Boolean) : CheckerResult[ParameterType] = {
    IdentParser(rawVarId) match {
      case IdentParser.Success(rawIdent, _) => {
        if (qualifiedIdents(getEnvironment(module)) contains rawIdent.toString) {
          CheckerFailure("Identifier " ++ rawIdent.toString ++ " exists in current scope.")
        } else {
          for {
            ident <- processRawIdentifier(module, rawIdent)
          } yield {
            val param = newParameter(Variable(ident, shell, isThin))
            module.appendEntry(param)
            param
          }
        }
      }
      case _ : IdentParser.NoSuccess =>
        CheckerFailure("Invalid variable identifier: " ++ rawVarId)
    }
  }

  def appendLift(module : ModuleType, rawBdryId : String, nook : Nook) : CheckerResult[LiftType] = {
    IdentParser(rawBdryId) match {
      case IdentParser.Success(rawIdent, _) => {
        if (qualifiedIdents(getEnvironment(module)) contains rawIdent.toString) {
          CheckerFailure("Identifier " ++ rawIdent.toString ++ " exists in current scope.")
        } else {
          for {
            ident <- processRawIdentifier(module, rawIdent)
          } yield {
            val lift = newLift(Filler(ident, nook))
            module.appendEntry(lift)
            lift
          }
        }
      }
      case _ : IdentParser.NoSuccess =>
        CheckerFailure("Invalid variable identifier: " ++ rawBdryId)
    }
  }

  //============================================================================================
  // ENVIRONMENT MANAGEMENT
  //

  def getLocalEnvironment(scope : Scope) : Seq[EnvironmentEntryType] = {
    scope.entries flatMap {
      case param : Parameter => Seq(newIdentifierEntry(param))
      case lift : Lift => 
        Seq(newIdentifierEntry(lift),
          newIdentifierEntry(lift.fillerEntry))
      case imprt : Import => {
        val importEnv = getLocalEnvironment(imprt)

        if (imprt.isOpen) {
          importEnv
        } else {
          Seq(newGroupEntry(imprt, importEnv))
        }
      }
      case subMod : Module => {
        val subModEnv = getLocalEnvironment(subMod)
        Seq(newGroupEntry(subMod, subModEnv))
      }
    }
  }

  def getEnclosingEnvironment(scope : Scope) : Seq[EnvironmentEntryType] = {
    scope.parentScope match {
      case None => Seq.empty
      case Some(prnt) => {
        // select the entries of the parent up the this guy.
        val myIndex = prnt.entries.indexOf(scope)
        val mySiblings = prnt.entries.slice(0, myIndex)
        val env = getEnclosingEnvironment(prnt)

        val siblingEnv =
          mySiblings flatMap {
            case param : Parameter => Seq(newIdentifierEntry(param))
            // Here you just add a second identifier entry for the boundary
            case lift : Lift => 
              Seq(newIdentifierEntry(lift),
              newIdentifierEntry(lift.fillerEntry))
            case imprt : Import => {
              val importEnv = getLocalEnvironment(imprt)

              if (imprt.isOpen) {
                importEnv
              } else {
                Seq(newGroupEntry(imprt, importEnv))
              }
            }
            case module : Module => Seq.empty
          }

        env ++ siblingEnv
      }
    }
  }

  def getEnvironment(scope : Scope) : Seq[EnvironmentEntryType] =
    getEnclosingEnvironment(scope) ++ getLocalEnvironment(scope)

  def lookupIdentifier(name : String, scope : Scope) : CheckerResult[IdentifierType] = {
    (identifierSeq(getEnvironment(scope)) find (entry => entry.name == name)) match {
      case None => CheckerFailure("Identifier lookup failed.")
      case Some(e) => CheckerResult(e)
    }
  }

  def identifierSeq(env : Seq[EnvironmentEntry]) : Seq[IdentifierType] =
    env flatMap {
      case idEntry : IdentifierEntry => Seq(idEntry.liftIdentifier)  // why the explicit lift here?
      case grpEntry : GroupEntry => identifierSeq(grpEntry.entries)
    }

  def qualifiedIdents(env : Seq[EnvironmentEntry]) : Seq[String] = 
    env flatMap {
      case idEntry : IdentifierEntry => Seq(idEntry.qualifiedName)
      case grpEntry : GroupEntry => qualifiedIdents(grpEntry.entries)
    }

  def qualifiedModules(env : Seq[EnvironmentEntry]) : Seq[String] = 
    env flatMap {
      case idEntry : IdentifierEntry => Seq.empty
      case grpEntry : GroupEntry => 
        Seq(grpEntry.name) ++ qualifiedModules(grpEntry.entries)
    }

  //============================================================================================
  // UTILS
  //

  def verify(condition : Boolean, message : String) : CheckerResult[Unit] = 
    if (condition) {
      CheckerResult(())
    } else {
      CheckerFailure(message)
    }

  // Please make these monadic ...
  def sequence[A](steps : List[CheckerResult[A]]) : CheckerResult[List[A]] =
    steps match {
      case Nil => CheckerResult(Nil)
      case s :: ss =>
        for {
          prevSteps <- sequence(ss)
          curStep <- s
        } yield (curStep :: prevSteps)
    }

  def seqSeq[A](steps : Seq[CheckerResult[A]]) : CheckerResult[Seq[A]] = {
    if (steps.isEmpty) {
      CheckerSuccess(Seq.empty)
    } else {
      for {
        tailResult <- seqSeq(steps.tail)
        headResult <- steps.head
      } yield { headResult +: tailResult }
    }
  }

  def shapeSequence[A](shape : NCell[CheckerResult[A]]) : CheckerResult[NCell[A]] =
    CheckerResult(
      shape map {
        case CheckerSuccess(a) => a
        case CheckerFailure(cause) => throw new RuntimeException(cause)
      }
    )
}

