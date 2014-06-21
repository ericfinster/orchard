/**
  * JavaFXSVGGallery.scala - Abstract base class for SVG Galleries in JavaFX
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx.svg

import scala.collection.JavaConversions._

import scalafx.Includes._
import scalafx.collections.ObservableBuffer
import scalafx.scene.web.WebEngine

import javafx.concurrent.Worker.State
import javafx.beans.value.ChangeListener
import javafx.beans.value.ObservableValue

import orchard.core.ui._
import orchard.core.svg._

abstract class JavaFXSVGGallery[A](labelEngine : WebEngine) extends Gallery[A] {

  override type PanelType <: JavaFXSVGPanel[A]

  protected val myPanels = new ObservableBuffer[PanelType]

  var onRenderFinished : Unit => Unit = (_ => ())

  def panelLayoutData : (Double, Double, List[(PanelType, Double, Double)]) = {

    // Problem: if the panel consists of a single cell, then the width
    // of the edges must also be taken into account, otherwise they get
    // chopped.  How can I fix this issue???

    var maxHeight : Double = 0.0

    panels foreach (panel => {
      maxHeight = Math.max(maxHeight, panel.panelHeight)
    })

    var curX = 0.0

    val panelData = 
      panels map (panel => {
        val thisHeight = panel.baseCell.height
        val curY = (maxHeight - thisHeight) / 2
        val res = (panel, curX, curY)
        curX += panel.panelWidth + 20
        res
      })

    // We can return the width and height here as well ....
    (curX, maxHeight, panelData)
  }

  def panels : List[PanelType] = myPanels.toList
  def newPanel(i : Int) : PanelType

  def appendPanel(panel : PanelType) = myPanels += panel

  override def renderAll = {

    val doSuperRenderAll : Unit => Unit = (_ => super.renderAll )
    val galleryProofSheet = <svg>{panels map (p => p.labelProofSheet)}</svg>

    labelEngine.getLoadWorker.stateProperty.addListener(
      new ChangeListener[State]{
        def changed(ov : ObservableValue[_ <: State], oldState : State, newState : State) {
          newState match {
            case State.SUCCEEDED => {
              panels foreach (panel => { 
                panel.setLabelSizes
                panel.render
              })

              onRenderFinished()
            }
            case _ => ()
          }
        }
      })

    labelEngine.loadContent(galleryProofSheet.toString)
  }

  def initialize = {
    reactTo(complex)
    myPanels ++= { for { i <- Range(0, complex.baseCells.length) } yield { newPanel(i) } }
  }

}
