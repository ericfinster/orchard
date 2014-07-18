/**
  * LiftGallery.scala - An SVG Gallery for Lift
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.lift

import scala.collection.mutable.ListBuffer

import orchard.core.svg._
import orchard.core.cell._
import orchard.core.complex._

class LiftGallery[A](seed : NCell[A]) extends SVGGallery[A] {

  type PanelType = LiftPanel[A]

  val complex : SimpleMutableComplex[A] = new SimpleMutableComplex(seed)

  def panels : List[LiftPanel[A]] = myPanels.toList
  val myPanels : ListBuffer[LiftPanel[A]] = ListBuffer.empty

  def newPanel(index : Int) = new LiftPanel(complex, index)

  def initialize = {
    reactTo(complex)
    myPanels ++= { for { i <- Range(0, complex.baseCells.length) } yield { newPanel(i) } }
  }

  initialize

}
