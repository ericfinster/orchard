/**
  * JCarousel.scala - Some traits and classes wrapping the carousel api
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.js.plugins

import scala.language.implicitConversions

import scala.scalajs._
import org.scalajs.jquery.JQuery

trait JCarousel extends JQuery {

  def jcarousel() : JCarousel = ???
  def jcarousel(data : js.Any) : JCarousel = ???
  def jcarousel(data : js.Any, element : js.Any) : JCarousel = ???
  def jcarouselControl(data : js.Any) : JCarousel = ???
  def jcarouselPagination() : JCarousel = ???
  def jcarouselPagination(data : js.Any) : JCarousel = ???

}

object JCarousel {

  implicit def jq2Carousel(jq : JQuery): JCarousel = 
    jq.asInstanceOf[JCarousel]

}
