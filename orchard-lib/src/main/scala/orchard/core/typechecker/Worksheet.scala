/**
  * Worksheet.scala - The Worksheet Trait
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.typechecker

import orchard.core.cell._

trait Worksheet {

  def toNCell : NCell[ExpressionMarker]

  def selectCell(addr : CellAddress) : Option[Unit]
  def appendToSelection(addr : CellAddress) : Option[Unit]

  def delete : Option[Unit]

  

}
