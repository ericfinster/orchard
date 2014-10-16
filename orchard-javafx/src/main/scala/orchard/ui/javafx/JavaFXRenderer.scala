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

class JavaFXRenderer(editor : JavaFXEditor) extends Renderer[Int] {

  def arcRadius : Double = 4.0
  def externalPadding : Double = 5.0
  def internalPadding : Double = 5.0
  def halfLeafWidth : Double = 10.0
  def halfStrokeWidth : Double = 1.0

  def getLabelBBox(a : Int) : BBox = {

    val labelText = new Text(a.toString)

    val bbox = new BBox {

      val width : Double = labelText.layoutBounds().width
      val height : Double = labelText.layoutBounds().height

    }

    editor.consoleWrite("Rendered text for: " ++ a.toString)
    editor.consoleWrite("Width : " ++ bbox.width.toString)
    editor.consoleWrite("Height : " ++ bbox.height.toString)

    bbox

  }

}
