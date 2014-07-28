/**
  * Checker.scala - The main checker class for Orchard
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.checker

class Checker(val rootModule : Module) {


}

class CheckerModuleDescription extends ModuleDescription {

  var address : Vector[Int] = Vector.empty

  def name : String = "Untitled"

  def insertEntryAt(e : ModuleEntry, i : Int) : Unit = ()

}
