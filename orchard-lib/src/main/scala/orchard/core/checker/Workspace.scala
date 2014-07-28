/**
  * Workspace.scala - A workspace with a type checker and worksheets
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.checker

import scala.collection.mutable.HashMap

import orchard.core.util._

import ErrorM._

class Workspace {

  val worksheetMap : HashMap[Int, Worksheet] = HashMap.empty

  def newWorksheet : Worksheet = {
    val worksheet = Worksheet()
    worksheetMap(worksheet.hashCode) = worksheet
    worksheet
  }
    
  // Now what?  The main thing we want to do is to assume variables.  That's the
  // goal right now.  So I think to do this, we are going to need to start outlining
  // the type checker as well.

}
