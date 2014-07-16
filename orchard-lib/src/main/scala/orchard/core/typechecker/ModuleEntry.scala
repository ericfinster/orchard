/**
  * ModuleEntry.scala - Internal syntax for modules
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.typechecker

sealed trait ModuleEntry
case class Module(name : String, entries : Vector[ModuleEntry]) extends ModuleEntry
case class ExpressionEntry(expr : Expression) extends ModuleEntry
