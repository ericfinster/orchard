/**
  * Framework.scala - Frameworks
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.checker

import orchard.core.cell._
import orchard.core.util._
import orchard.core.complex._

import ErrorM._

// trait FrameworkEntry {

//   def isEmpty : Boolean
//   def isThin : Boolean

// }

// trait Framework[A <: FrameworkEntry] extends CellComplex[A] {

//   type CellType <: FrameworkCell
//   type ExtractionType <: Framework[A]

//   def extract(cell : CellType) : ExtractionType

//   trait FrameworkCell extends ComplexCell { thisCell : CellType =>

//     def isThin : Boolean = item.isThin
//     def isEmpty : Boolean = item.isEmpty
//     def isFull : Boolean = ! isEmpty

//     def isNook : Boolean = isInNook || isOutNook

//     def isInNook : Boolean
//     def isOutNook : Boolean
//     def isExposedNook : Boolean

//     def isCompleteShell : Boolean

//     def isShell : Boolean = isEmpty && isCompleteShell 
//     def isComplete : Boolean = isFull && isCompleteShell

//     def isFillable : Boolean

//   }

// }
