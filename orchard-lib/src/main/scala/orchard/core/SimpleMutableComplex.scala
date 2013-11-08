/**
  * SimpleMutableComplex.scala - A Simple Implementation of mutability
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.WeakHashMap

import Util._

class SimpleMutableComplex[A](seed : NCell[A]) extends MutableComplex[A] {

  type CellType = SimpleMutableCell

  //============================================================================================
  // COMPLEX IMPLEMENTATION
  //

  def newCell(item : A) = new SimpleMutableCell(item)

  protected val myBaseCells : ListBuffer[SimpleMutableCell] = new ListBuffer
  myBaseCells ++= seed.regenerateFrom(ComplexGenerator).value.targets

  def baseCells : List[CellType] = myBaseCells.toList
  def appendBaseCell(cell : SimpleMutableCell) : Unit = myBaseCells += cell
  def setBaseCell(i : Int, cell : SimpleMutableCell) = myBaseCells(i) = cell

  //============================================================================================
  // CELL IMPLEMENTATION
  //

  class SimpleMutableCell(var item : A) extends MutableCell

}
