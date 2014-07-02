/**
  * Environment.scala - Routines for building the local environment
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

sealed trait EnvironmentElement
case class IdentifierElement(name : String) extends EnvironmentElement
case class ModuleElement(name : String) extends EnvironmentElement
