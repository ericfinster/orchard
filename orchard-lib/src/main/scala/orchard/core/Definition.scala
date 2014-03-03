/**
  * Definition.scala - A class encapsulating a definition
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

import Environment._

class Definition(
  val name : String,
  val stabilityLevel : Option[Int],
  val invertibilityLevel : Option[Int],
  val unicityLevel : Option[Int], 
  val environment : Seq[NCell[Expression]]
) {

  override def toString = name

}
