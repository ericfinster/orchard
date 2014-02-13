/**
  * DefinitonWorkspace.scala - A workspace implementation for definitons
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

import scala.collection.mutable.Buffer

class DefinitionWorkspace(
  val name : String,
  val stabilityLevel : Option[Int],
  val invertibilityLevel : Option[Int],
  val unicityLevel : Option[Int]
) extends Workspace {


}
