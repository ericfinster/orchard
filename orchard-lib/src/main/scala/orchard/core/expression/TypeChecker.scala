/**
  * TypeChecker.scala - A type checker for Orchard
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

abstract class TypeChecker 
    extends ModuleModule
    with EnvironmentModule
    with ExpressionModule 
    with FrameworkModule {

  type EditorType <: Editor
  def editor : EditorType

  def rootModule : ModuleType

  def appendSubmodule(module : ModuleType, rawModuleId : String) : CheckerResult[ModuleType] = {
    ModuleIdentParser(rawModuleId) match {
      case ModuleIdentParser.Success(moduleId, _) => {
        if (qualifiedModules(getEnvironment(module)) contains moduleId) {
          CheckerFailure("Submodule " ++ moduleId ++ " already exists.")
        } else {
          val subMod = newModule(moduleId)
          module.appendEntry(subMod)
          CheckerSuccess(subMod)
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

  def processRawIdentifier(scope : Scope, rawIdent : RawIdentifier) : CheckerResult[Identifier] = {
    val idents : List[CheckerResult[Identifier]] = 
      rawIdent.tokens map {
        case RawLiteral(lit) => CheckerSuccess(LiteralIdentifier(lit))
        case RawReference(ref) =>
          for {
            resultRef <- lookupIdentifier(ref, scope)
          } yield ReferenceIdentifier(resultRef)
      }

    for {
      newIdents <- sequence(idents)
    } yield CompoundIdentifier(newIdents)
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
      case Some(e) => CheckerSuccess(e)
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
      CheckerSuccess(())
    } else {
      CheckerFailure(message)
    }

  def sequence[A](steps : List[CheckerResult[A]]) : CheckerResult[List[A]] =
    steps match {
      case Nil => CheckerSuccess(Nil)
      case s :: ss =>
        for {
          prevSteps <- sequence(ss)
          curStep <- s
        } yield (curStep :: prevSteps)
    }

}

