/**
  * JavaFXGallery.scala - An abstract JavaFXGallery
  * 
  * @author Eric Finster
  * @version 0.1
  */

package orchard.ui.javafx

import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

import orchard.core._

abstract class JavaFXGallery[A] extends Spinner with Gallery[A] {

  override type PanelType <: JavaFXPanel[A]

  //============================================================================================
  // UI INITIALIZATION
  //

  getStyleClass().add("javafx-gallery")
  
  private val myPanels = new ListBuffer[PanelType]

  def panels : List[PanelType] = myPanels.toList
  def newPanel(i : Int) : PanelType

  def appendPanel(panel : PanelType) = {
    myPanels += panel
    hbox.getChildren.add(panel)
  }

  def refreshAll = {
    panels foreach (_.refresh)
  }

  def initialize = {
    reactTo(complex)
    myPanels ++= { for { i <- Range(0, complex.baseCells.length) } yield { newPanel(i) } }
    hbox.getChildren.addAll(panels)
  }
}
