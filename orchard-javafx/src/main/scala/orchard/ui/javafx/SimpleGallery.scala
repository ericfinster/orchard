/**
  * SimpleGallery.scala - A Simple Gallery Implementation
  * 
  * @author Eric Finster
  * @version 0.1
  */

package orchard.ui.javafx

import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

import orchard.core._

class SimpleGallery[A](seed : NCell[A]) extends JavaFXGallery[A] {

  type PanelType = SimplePanel[A]

  val complex : SimpleMutableComplex[A] = new SimpleMutableComplex(seed)

  private val myPanels = new ListBuffer[SimplePanel[A]]
  myPanels ++= { for { i <- Range(0, complex.baseCells.length) } yield { new SimplePanel(complex, i) } }

  def panels : List[PanelType] = myPanels.toList

  hbox.getChildren.addAll(panels)

}
