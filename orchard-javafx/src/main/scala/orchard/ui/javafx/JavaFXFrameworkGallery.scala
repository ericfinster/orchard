/**
  * JavaFXFrameworkGallery.scala - A simple gallery for displaying frameworks
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.scene.text.Text
import scalafx.scene.layout.Region

import orchard.core.ui._
import orchard.core.cell._
import orchard.core.editor._
import orchard.core.complex._
import orchard.core.expression._

import javafx.{scene => jfxs}

class FrameworkGallery(val complex : Framework[Option[Expression]]) extends SpinnerGallery[Option[Expression]] { thisGallery =>

  def this(seed : NCell[Option[Expression]]) = this(new SimpleFramework(seed))

  type PanelType = FrameworkPanel

  def newPanel(i : Int) : FrameworkPanel = {
    val panel = new FrameworkPanel(complex, i)
    reactTo(panel)
    panel
  }

  initialize

}


