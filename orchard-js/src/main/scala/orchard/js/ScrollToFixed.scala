/**
  * ScrollToFixed.scala - Wrapper for splitter jquery div fixing library
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.js

import scala.language.implicitConversions
import scala.scalajs._
import org.scalajs.jquery._

trait ScrollToFixed extends JQuery {

  def scrollToFixed() : ScrollToFixed = ???
  def scrollToFixed(conf : js.Any) : ScrollToFixed = ???

}

object ScrollToFixed {

  implicit def jq2ScrollToFixed(jq : JQuery) : ScrollToFixed = 
    jq.asInstanceOf[ScrollToFixed]

}
