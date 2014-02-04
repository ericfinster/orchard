/**
  * DefinitionBuilder.scala - A trait which encapsulates the requirements for building a definition
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

import scala.collection.mutable.Buffer

trait DefinitionBuilder {

  def environment : Buffer[NCell[Expression]]

  def getFromEnvironment(id : String) : Option[NCell[Expression]] = {
    environment find (expr => expr.value.id == id)
  }

  def envContains(id : String) : Boolean = {
    environment exists (expr => expr.value.id == id)
  }

  var truncationLevel : Int
  var contractibilityLevel : Int

}
