/**
  * CellEvent.scala - Cell Events
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

trait CellEvent

case object RequestSelected extends CellEvent
case object RequestDeselected extends CellEvent
case object RequestHovered extends CellEvent
case object RequestUnhovered extends CellEvent

case class CellEntered[A](cell : Panel[A]#CellType) extends CellEvent
case class CellExited[A](cell : Panel[A]#CellType) extends CellEvent
case class CellClicked[A](cell : Panel[A]#CellType) extends CellEvent
case class CellCtrlClicked[A](cell : Panel[A]#CellType) extends CellEvent
case class CellDoubleClicked[A](cell : Panel[A]#CellType) extends CellEvent

sealed trait ComplexEvent extends CellEvent
case object ComplexExtended extends ComplexEvent

