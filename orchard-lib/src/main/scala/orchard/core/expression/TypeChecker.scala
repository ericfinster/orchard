/**
  * TypeChecker.scala - A type checker for Orchard
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

abstract class TypeChecker {
  thisChecker : ModuleSystem with EnvironmentSystem =>

  def rootModule : CheckerResult[Module]

  def appendSubmodule(module : Module, name : String) : CheckerResult[Module] = {
    if (qualifiedModules(getLocalEnvironment(module)) contains name) {
      CheckerFailure("Submodule " ++ name ++ " already exists.")
    } else {
      val subMod = newModule(name)
      module.appendEntry(subMod)
      CheckerSuccess(subMod)
    }
  }

  def appendParameter(module : Module, variable : Variable) : CheckerResult[Parameter] = {
    // This should parse an identifier string instead of just taking the variable as a
    // parameter to the function.  So the shell should be included as well ...

    if (qualifiedIdents(getEnvironment(module)) contains variable.ident.toString) {
      CheckerFailure("Identifier " ++ variable.ident.toString ++ " already exists in the current scope.")
    } else {
      ???
    }
  }

  //============================================================================================
  // ENVIRONMENT MANAGEMENT
  //

  def getLocalEnvironment(scope : Scope) : Seq[EnvironmentEntryType] = {
    scope.entries flatMap {
      case param : Parameter => Seq(newIdentifierEntry(param.name))
      case lift : Lift => Seq(newIdentifierEntry(lift.name))
      case imprt : Import => {
        val importEnv = getLocalEnvironment(imprt)

        if (imprt.isOpen) {
          importEnv
        } else {
          Seq(newGroupEntry(imprt.name, importEnv))
        }
      }
      case subMod : Module => {
        val subModEnv = getLocalEnvironment(subMod)
        Seq(newGroupEntry(subMod.name, subModEnv))
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
            case param : Parameter => Seq(newIdentifierEntry(param.name))
            case lift : Lift => Seq(newIdentifierEntry(lift.name))
            case imprt : Import => {
              val importEnv = getLocalEnvironment(imprt)

              if (imprt.isOpen) {
                importEnv
              } else {
                Seq(newGroupEntry(imprt.name, importEnv))
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

}

case class CheckerSuccess[+A](result : A) extends CheckerResult[A] {

  def map[B](f : A => B) : CheckerResult[B] =
    CheckerSuccess(f(result))

  def flatMap[B](f : A => CheckerResult[B]) : CheckerResult[B] =
    f(result)

  def filter(f : A => Boolean) : CheckerResult[A] = 
    if (f(result)) this else CheckerFailure("Result was filtered")

}

case class CheckerFailure(cause : String) extends CheckerResult[Nothing] {

  def map[B](f : Nothing => B) : CheckerResult[B] = this
  def flatMap[B](f : Nothing => CheckerResult[B]) : CheckerResult[B] = this
  def filter(f : Nothing => Boolean) : CheckerResult[Nothing] = this

}
