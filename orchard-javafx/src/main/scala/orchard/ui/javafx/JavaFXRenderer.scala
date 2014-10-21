/**
  * JavaFXRenderer.scala - JavaFX Renderer Implementation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.Includes._

import scalafx.geometry._
import scalafx.scene.layout._
import scalafx.scene.control._
import scalafx.scene.text._

import orchard.core.tree._

class JavaFXRenderer(editor : JavaFXEditor) extends Renderer[Double, Int] {

  def arcRadius : Double = 4.0
  def externalPadding : Double = 5.0
  def internalPadding : Double = 5.0
  def halfLeafWidth : Double = 10.0
  def halfStrokeWidth : Double = 1.0

  def createExternalBox(a : Int) : ExternalBox = {

    val labelText = new Text(a.toString)

    new ExternalBox {

      var rootX : Double = 0.0
      var rootY : Double = 0.0

      def halfLabelWidth : Double = labelText.layoutBounds().width / 2
      def halfLabelHeight : Double = labelText.layoutBounds().height / 2

    }

  }

  def createInternalBox(a : Int, layout : BoxLayout) = {

    val labelText = new Text(a.toString)

    new InternalBox {

      def interior : BoxLayout = layout

      var rootX : Double = 0.0
      var rootY : Double = 0.0

      def halfLabelWidth : Double = labelText.layoutBounds().width / 2
      def halfLabelHeight : Double = labelText.layoutBounds().height / 2

    }

  }

  def createEdge : Edge = new Edge { }

}
