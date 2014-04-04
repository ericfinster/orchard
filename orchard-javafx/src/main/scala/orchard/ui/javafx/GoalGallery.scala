/**
  * GoalGallery.scala - Gallery and panel definitions for goals
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.scene.text.Text
import scalafx.scene.layout.Region

import orchard.core._

import javafx.{scene => jfxs}

trait GoalEnvironment { thisEnvironment : JavaFXSubstitutionWorkspace =>

//   class GoalPanel(val complex : GoalFramework, val baseIndex : Int) 
//       extends ZoomPanel[GoalAddr] 
//       with MutablePanel[GoalAddr] { thisPanel =>

//     type CellType = GoalCell
//     type EdgeType = GoalEdge

//     type ComplexType = GoalFramework

//     class GoalCell(val owner : complex.GoalFrameworkCell) extends JavaFXCell with MutablePanelCell {

//       def expression = owner.expression

//       def renderLabel : jfxs.Node = {
//         val labelNode =
//           expression match {
//             case None => new Region { prefWidth = 10 ; prefHeight = 10 }
//             case Some(expr) => new Text(expr.id)
//           }

//         labelNode.layoutBounds onChange { thisPanel.refresh }
//         pane.getChildren.setAll(labelNode)
//         labelNode
//       }

//       def getStyleString = 
//         expression match {
//           case None => "empty"
//           case Some(expr) => expr.styleString
//         }

//       renderCell

//       override def onEventEmitted(ev : CellEvent) = {
//         ev match {
//           case complex.ChangeEvents.ItemChangedEvent(oldItem) => { renderCell ;  super.onEventEmitted(ev) }
//           case CellEntered(cell) => { owner.emitToFaces(RequestCellHovered) ; owner.emit(RequestEdgeHovered) }
//           case CellExited(cell) => { owner.emitToFaces(RequestCellUnhovered) ; owner.emit(RequestEdgeUnhovered) }
//           case _ => super.onEventEmitted(ev)
//         }
//       }
//     }

//     class GoalEdge(val owner : complex.GoalFrameworkCell) extends JavaFXEdge with MutablePanelEdge

//     def newCell(owner : complex.GoalFrameworkCell) : GoalCell = {
//       val cell = new GoalCell(owner)
//       owner.registerPanelCell(thisPanel)(cell)
//       reactTo(cell)
//       cell
//     }

//     def newEdge(owner : complex.GoalFrameworkCell) : GoalEdge = {
//       val edge = new GoalEdge(owner)
//       owner.registerPanelEdge(thisPanel)(edge)
//       reactTo(edge)
//       edge
//     }

//     //============================================================================================
//     // INITIALIZATION
//     //

//     var baseCell : GoalCell = newCell(complex.baseCells(baseIndex))

//     refreshPanelData
//     initializeChildren

//   }

//   class GoalGallery(val complex : GoalFramework) extends SpinnerGallery[GoalAddr] { thisGallery =>

//     type PanelType = GoalPanel

//     def newPanel(i : Int) : GoalPanel = {
//       val panel = new GoalPanel(complex, i)
//       reactTo(panel)
//       panel
//     }

//     initialize
//   }

}
