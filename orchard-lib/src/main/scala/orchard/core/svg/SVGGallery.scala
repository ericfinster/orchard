/**
  * SVGGallery.scala - Base trait for SVG Galleries
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.svg

import orchard.core.ui._
import orchard.core.complex._

import xml._

trait SVGGallery[A] extends Gallery[A] {

  type PanelType <: SVGPanel[A]


}
