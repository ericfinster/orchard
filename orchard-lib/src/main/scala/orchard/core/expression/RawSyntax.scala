/**
  * RawSyntax.scala - Raw serializable syntax
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

sealed trait ExpressionMarker
case object Empty extends ExpressionMarker
case class Marker(id : String, isThin : Boolean) extends ExpressionMarker

class Nook(framework : Framework[ExpressionMarker])
class Shell(framework : Framework[ExpressionMarker])


