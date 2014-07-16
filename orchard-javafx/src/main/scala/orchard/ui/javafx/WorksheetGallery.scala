/**
  * WorksheetGallery.scala - A Gallery for displaying Worksheets
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import orchard.core.typechecker._

class WorksheetGallery(val worksheet : WorksheetHandle) extends SpinnerGallery[ExpressionMarker] {

  type PanelType = WorksheetPanel

  var complex = worksheet.complex

  def newPanel(i : Int) : WorksheetPanel = {
    val panel = new WorksheetPanel(worksheet, i)
    reactTo(panel)
    panel
  }

  initialize

}

//   //============================================================================================
//   // EVENTS
//   //

//   override def onEventEmitted(ev : CellEvent) = {
//     val cmplx = complex

//     ev match {
//       case CellClicked(c) => {
//         val cell = c.owner.asInstanceOf[cmplx.CellType]

//         // for {
//         //   expr <- cell.expression
//         // } {
//         //   OrchardEditor.consoleMessage("Expression: " ++ expr.toString)
//         //   OrchardEditor.consoleMessage("Normalized expression: " ++ expr.normalize.toString)
//         // }
//       }

//       case _ => super.onEventEmitted(ev)
//     }
//   }

//   initialize

// }

