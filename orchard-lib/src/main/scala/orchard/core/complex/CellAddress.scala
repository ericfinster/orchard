/**
  * CellAddress.scala - Addresses inside a Cell
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.complex

sealed trait CellAddress
sealed trait AddressPrefix extends CellAddress

case object Immediate extends AddressPrefix { 
  override def toString = "*" 
}

case class Target(val prefix : AddressPrefix) extends AddressPrefix {
  override def toString = "Tgt : " ++ prefix.toString
}

case class Source(val prefix : AddressPrefix, val loc : List[Int]) extends CellAddress {
  override def toString = loc.toString ++ " : " ++ prefix.toString
}
