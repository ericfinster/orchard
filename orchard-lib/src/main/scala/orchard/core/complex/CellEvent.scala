/**
  * CellEvent.scala - Cell Events
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.complex

import orchard.core.ui._

trait CellEvent

case object RequestCellSelected extends CellEvent
case object RequestCellDeselected extends CellEvent
case object RequestCellHovered extends CellEvent
case object RequestCellUnhovered extends CellEvent

case object RequestEdgeSelected extends CellEvent
case object RequestEdgeDeselected extends CellEvent
case object RequestEdgeHovered extends CellEvent
case object RequestEdgeUnhovered extends CellEvent

case class CellEntered[A](cell : Panel[A]#CellType) extends CellEvent
case class CellExited[A](cell : Panel[A]#CellType) extends CellEvent
case class CellClicked[A](cell : Panel[A]#CellType) extends CellEvent
case class CellCtrlClicked[A](cell : Panel[A]#CellType) extends CellEvent
case class CellDoubleClicked[A](cell : Panel[A]#CellType) extends CellEvent

sealed trait ComplexEvent extends CellEvent
case object ComplexExtended extends ComplexEvent

sealed trait PanelEvent extends CellEvent
case object PanelClicked extends PanelEvent
