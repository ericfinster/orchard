/**
  * Editor.scala - A trait for editors
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

import Environment._

trait Editor {

  def withAssumptionInfo(thinHint : Boolean,
                         forceThin : Boolean,
                         handler : (String, Boolean) => Unit) : Unit 

  def withFillerIdentifiers(handler : (String, String) => Unit) : Unit
  def withFillerIdentifier(handler : String => Unit) : Unit

}

