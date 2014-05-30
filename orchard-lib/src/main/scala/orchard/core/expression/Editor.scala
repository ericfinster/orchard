/**
  * Editor.scala - Editor trait 
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.expression

trait Editor {

  def withAssumptionInfo(thinHint : Boolean,
                         forceThin : Boolean,
                         handler : (String, Boolean) => Unit) : Unit 

  def withFillerIdentifier(handler : String => Unit) : Unit

  // def withFillerIdentifiers(handler : (String, String) => Unit) : Unit
  // def withRenameIdentifier(expr : Expression, handler : String => Unit) : Unit

  def consoleWrite(str : String) : Unit 
  def consoleMessage(str : String) : Unit
  def consoleError(str : String) : Unit
  def consoleDebug(str : String) : Unit

}
