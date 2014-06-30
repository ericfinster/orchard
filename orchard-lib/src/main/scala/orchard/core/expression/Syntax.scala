/**
  * Syntax.scala - Definitions of Syntactic Elements
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

import orchard.core.cell._

sealed trait Statement
case class ModuleDefinition(name : String, body : List[Statement]) extends Statement
case class ModuleInstantiation(moduleIdent : String, moduleName : String, bindings : Map[String, String]) extends Statement
case class ParameterDefinition(ident : Identifier, shell : Shell) extends Statement
case class LiftDefinition(ident : Identifier, nook : Nook) extends Statement

sealed trait Expression
sealed trait Identifier

class Nook(ncell : NCell[Option[Expression]])
class Shell(ncell : NCell[Option[Expression]])




