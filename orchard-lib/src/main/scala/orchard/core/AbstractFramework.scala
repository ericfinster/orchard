/**
  * AbstractFramework.scala - An abstract expression framework
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.mutable.WeakHashMap
import scala.collection.mutable.Buffer

abstract class AbstractFramework(seed : NCell[Option[Expression]]) 
    extends AbstractMutableComplex[Option[Expression]](seed) 
    with ExpressionFramework[Option[Expression]] {

  type CellType <: AbstractFrameworkCell

  // def variables : Seq[NCell[Expression]] = {
  //   val buf = Buffer.empty[NCell[Expression]]
  //   forAllCells (cell => if (cell.isVariable) { buf += cell.toExpressionCell })
  //   buf
  // }

  // def fillers : Seq[NCell[Expression]] = {
  //   val buf = Buffer.empty[NCell[Expression]]
  //   forAllCells (cell => if (cell.isFiller) { buf += cell.toExpressionCell })
  //   buf
  // }

  // def fillerFaces : Seq[NCell[Expression]] = {
  //   val buf = Buffer.empty[NCell[Expression]]
  //   forAllCells (cell => if (cell.isFillerFace) { buf += cell.toExpressionCell })
  //   buf
  // }

  abstract class AbstractFrameworkCell(var item : Option[Expression])
      extends AbstractMutableCell with ExpressionFrameworkCell { thisCell : CellType =>

    // def exprItem = item

    // def isVariable : Boolean = 
    //   item match {
    //     case Some(Variable(_, _, _)) => true
    //     case _ => false
    //   }

    // def isInterior : Boolean = 
    //   item match {
    //     case Some(Interior(_, _)) => true
    //     case _ => false
    //   }

    // def isFillerFace : Boolean =
    //   item match {
    //     case Some(FillerFace(_, _, _)) => true
    //     case _ => false
    //   }

    // def isUnicityFiller : Boolean = 
    //   item match {
    //     case Some(UnicityFiller(_)) => true
    //     case _ => false
    //   }
  }

}

