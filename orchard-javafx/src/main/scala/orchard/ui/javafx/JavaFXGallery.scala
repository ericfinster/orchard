/**
  * JavaFXGallery.scala - An abstract JavaFXGallery
  * 
  * @author Eric Finster
  * @version 0.1
  */

package orchard.ui.javafx

import scala.collection.JavaConversions._

import scalafx.Includes._
import scalafx.collections.ObservableBuffer

import orchard.core._

trait JavaFXGallery[A] extends Gallery[A] {

  override type PanelType <: JavaFXPanel[A]

  protected val myPanels = new ObservableBuffer[PanelType]

  def panels : List[PanelType] = myPanels.toList
  def newPanel(i : Int) : PanelType

  def appendPanel(panel : PanelType) = myPanels += panel
  def refreshAll = panels foreach (_.refresh)

  def initialize = {
    reactTo(complex)
    myPanels ++= { for { i <- Range(0, complex.baseCells.length) } yield { newPanel(i) } }
  }

}
