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
    with WorkspaceModule
    with FrameworkModule
    with WorksheetModule {

  type EditorType <: Editor
  def editor : EditorType

  def rootModule : CheckerResult[Module]

  def appendSubmodule(module : ModuleType, rawModuleId : String) : CheckerResult[Module] = {
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

  def appendParameter(module : ModuleType, rawVarId : String, shell : Shell, isThin : Boolean) : CheckerResult[Parameter] = {
    IdentParser(rawVarId) match {
      case IdentParser.Success(rawIdent, _) => {
        if (qualifiedIdents(getEnvironment(module)) contains rawIdent.toString) {
          CheckerFailure("Identifier " ++ rawIdent.toString ++ " exists in current scope.")
        } else {
          for {
            ident <- processRawIdentifier(module, rawIdent)
          } yield {
            val param = newParameter(ident, shell, isThin)
            module.appendEntry(param)
            param
          }
        }
      }
      case _ : IdentParser.NoSuccess =>
        CheckerFailure("Invalid variable identifier: " ++ rawVarId)
    }
  }

  def processRawIdentifier(scope : Scope, rawIdent : RawIdentifier) : CheckerResult[Identifier] = {
    // In this routine, references in the raw identifier should be resolved to 
    // actual references in the current scope ...


    // val idents = rawIdent.tokens flatMap {
    //   case RawLiteral(lit) => Some(LiteralIdentifier(lit))
    //   case RawReference(ref) =>
    //     variables find (p => p.id == ref) match {
    //       case None => { editor.consoleError("Unresolved reference: " ++ ref) ; None }
    //       case Some(v) => Some(v.ident)
    //     }
    // }

    // if (idents.length < rawIdent.tokens.length) {
    //   editor.consoleError("Identifier processing failed.")
    //   None
    // } else {
    //   Some(idents)
    // }
    ???
  }


  //============================================================================================
  // ENVIRONMENT MANAGEMENT
  //

  def getLocalEnvironment(scope : Scope) : Seq[EnvironmentEntryType] = {
    scope.entries flatMap {
      case param : Parameter => Seq(newIdentifierEntry(param.name, param))
      case lift : Lift => Seq(newIdentifierEntry(lift.name, lift))
      case imprt : Import => {
        val importEnv = getLocalEnvironment(imprt)

        if (imprt.isOpen) {
          importEnv
        } else {
          Seq(newGroupEntry(imprt.name, imprt, importEnv))
        }
      }
      case subMod : Module => {
        val subModEnv = getLocalEnvironment(subMod)
        Seq(newGroupEntry(subMod.name, subMod, subModEnv))
      }
    }
  }

  def getEnclosingEnvironment(scope : Scope) : Seq[EnvironmentEntry] = {
    scope.parentScope match {
      case None => Seq.empty
      case Some(prnt) => {
        // select the entries of the parent up the this guy.
        val myIndex = prnt.entries.indexOf(scope)
        val mySiblings = prnt.entries.slice(0, myIndex)
        val env = getEnclosingEnvironment(prnt)

        val siblingEnv =
          mySiblings flatMap {
            case param : Parameter => Seq(newIdentifierEntry(param.name, param))
            case lift : Lift => Seq(newIdentifierEntry(lift.name, lift))
            case imprt : Import => {
              val importEnv = getLocalEnvironment(imprt)

              if (imprt.isOpen) {
                importEnv
              } else {
                Seq(newGroupEntry(imprt.name, imprt, importEnv))
              }
            }
            case module : Module => Seq.empty
          }

        env ++ siblingEnv
      }
    }
  }

  def getEnvironment(scope : Scope) : Seq[EnvironmentEntry] =
    getEnclosingEnvironment(scope) ++ getLocalEnvironment(scope)

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

}

sealed trait CheckerResult[+A] {

  def map[B](f : A => B) : CheckerResult[B]
  def flatMap[B](f : A => CheckerResult[B]) : CheckerResult[B]
  def filter(f : A => Boolean) : CheckerResult[A]
  def foreach(f : A => Unit) : Unit

}

case class CheckerSuccess[+A](result : A) extends CheckerResult[A] {

  def map[B](f : A => B) : CheckerResult[B] =
    CheckerSuccess(f(result))

  def flatMap[B](f : A => CheckerResult[B]) : CheckerResult[B] =
    f(result)

  def filter(f : A => Boolean) : CheckerResult[A] = 
    if (f(result)) this else CheckerFailure("Result was filtered")

  def foreach(f : A => Unit) : Unit = 
    f(result)

}

case class CheckerFailure(cause : String) extends CheckerResult[Nothing] {

  def map[B](f : Nothing => B) : CheckerResult[B] = this
  def flatMap[B](f : Nothing => CheckerResult[B]) : CheckerResult[B] = this
  def filter(f : Nothing => Boolean) : CheckerResult[Nothing] = this
  def foreach(f : Nothing => Unit) : Unit = ()

}
