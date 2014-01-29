/**
  * Definition.scala - A class encapsulating a definition
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

class Definition {

  val environment : Map[String, NCell[Expression]] = new HashMap[String, NCell[Expression]]

}
