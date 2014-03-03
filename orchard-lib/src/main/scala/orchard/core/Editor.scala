/**
  * Editor.scala - A trait for editors
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

trait Editor {

  def withAssumptionInfo(deps : Seq[NCell[Expression]],
                         thinHint : Boolean,
                         forceThin : Boolean,
                         handler : (String, Boolean) => Unit) : Unit 

  def withFillerIdentifiers(deps : Seq[NCell[Expression]], handler : (String, String) => Unit) : Unit
  def withFillerIdentifier(deps : Seq[NCell[Expression]], handler : String => Unit) : Unit

}

