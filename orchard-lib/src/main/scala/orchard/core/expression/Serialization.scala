/**
  * Serialization.scala - Serialization Routines
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

import scala.xml._

trait SerializationModule { thisModule : TypeChecker =>

  def statementToXML(stmt : Statement) : CheckerResult[NodeSeq] = 
    stmt match {
      case ModuleDefinition(name, body) => ???
      case ModuleImport(_, _, _) => CheckerFailure("Not implemented")
      case ParameterDefinition(variable) => ???
      case LiftDefinition(filler) => ???
    }

  def expressionToXML(expr : Expression) : CheckerResult[NodeSeq] = 
    expr match {
      case Reference(entry) => ???
      case _ => ???
    }

  
}
