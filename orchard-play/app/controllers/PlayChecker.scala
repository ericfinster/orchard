/**
  * PlayChecker.scala - An implementation of the Checker trait as a controller in Play
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package controllers

import orchard.core.typechecker._

class PlayChecker extends Checker {

  var globalModule : Module = Module(new ModuleDefinition("Global", Vector.empty), Vector.empty)

}
