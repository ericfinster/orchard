/**
  * RawSyntax.scala - Raw serializable syntax
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

import orchard.core.cell._

trait SyntaxModule { thisModule : TypeChecker =>

  sealed trait Statement
  case class ModuleDefinition(name : String, body : Seq[Statement]) extends Statement
  case class ModuleImport(moduleIdent : String, moduleName : String, bindings : Map[String, String]) extends Statement
  case class ParameterDefinition(variable : Variable) extends Statement
  case class LiftDefinition(filler : Filler) extends Statement

  
}



