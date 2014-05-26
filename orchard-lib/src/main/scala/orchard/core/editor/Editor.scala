/**
  * Editor.scala - Editor trait 
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.editor

import orchard.core.expression._

trait Editor {

  def withAssumptionInfo(thinHint : Boolean,
                         forceThin : Boolean,
                         handler : (String, Boolean) => Unit) : Unit 

  def withFillerIdentifiers(handler : (String, String) => Unit) : Unit
  def withFillerIdentifier(handler : String => Unit) : Unit

  def withRenameIdentifier(expr : Expression, handler : String => Unit) : Unit

}
