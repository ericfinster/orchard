/**
  * Definition.scala - A class encapsulating a definition
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

import scala.collection.mutable.Buffer

import Environment._

class Definition(
  val name : String,
  val stabilityLevel : Option[Int],
  val invertibilityLevel : Option[Int],
  val unicityLevel : Option[Int], 
  val args : Seq[Expression[Seq[Int]]],
  val outputIndex : Int
) extends SequentialContextEnvironment {

  override def toString = name

  def contextBuffer = Buffer.empty ++= args

}
