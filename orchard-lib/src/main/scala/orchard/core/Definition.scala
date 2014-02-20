/**
  * Definition.scala - A class encapsulating a definition
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

import scala.collection.mutable.Buffer

class Definition(
  val name : String,
  val stabilityLevel : Option[Int],
  val invertibilityLevel : Option[Int],
  val unicityLevel : Option[Int], 
  val result : NCell[Expression],
  val environment : Seq[NCell[Expression]]
) extends Environment {

  type EnvironmentSeqType = Seq[NCell[Expression]]

  override def toString = name

}
