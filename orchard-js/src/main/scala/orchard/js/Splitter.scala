/**
  * Splitter.scala - Wrapper for splitter jquery split pane library
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.js

import scala.language.implicitConversions
import scala.scalajs._
import org.scalajs.jquery._

trait Splitter extends JQuery {

  def splitter() : Splitter = ???
  def splitter(conf : js.Object) = ???

}

object Splitter {

  implicit def jq2Splitter(jq : JQuery) : Splitter = 
    jq.asInstanceOf[Splitter]

}
