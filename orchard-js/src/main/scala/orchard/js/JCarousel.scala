/**
  * JCarousel.scala - Some traits and classes wrapping the carousel api
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.js

import scala.language.implicitConversions

import scala.scalajs._
import org.scalajs.jquery.JQuery

trait JQueryCarousel extends JQuery {

  def jcarousel() : JCarousel = ???
  def jcarousel(conf : js.Object) : JCarousel = ???
  def jcarouselControl(conf : js.Object) : Unit = ???

}

object JQueryCarousel {

  implicit def jq2Carousel(jq : JQuery): JQueryCarousel = 
    jq.asInstanceOf[JQueryCarousel]

}

trait JCarousel extends js.Object {

}
