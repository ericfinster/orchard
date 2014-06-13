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
import orchard.core.complex._
import orchard.core.expression._

import javafx.{scene => jfxs}

class FrameworkGallery(val complex : Framework[Option[Expression]]) extends SpinnerGallery[Option[Expression]] { thisGallery =>

  def this(seed : NCell[Option[Expression]]) = this(new SimpleFramework(seed))
  def this(expr : Expression) = this(expr.ncell map (Some(_)))

  type PanelType = FrameworkPanel

  def newPanel(i : Int) : FrameworkPanel = {
    val panel = new FrameworkPanel(complex, i)
    reactTo(panel)
    panel
  }

    //============================================================================================
    // EVENTS
    //

    override def onEventEmitted(ev : CellEvent) = {
      val cmplx = complex

      ev match {
        case CellClicked(c) => {
          val cell = c.owner.asInstanceOf[cmplx.CellType]

          for {
            expr <- cell.expression
          } {
            OrchardEditor.consoleMessage("Expression: " ++ expr.toString)
          }
        }

        case _ => super.onEventEmitted(ev)
      }
    }

  initialize

}


