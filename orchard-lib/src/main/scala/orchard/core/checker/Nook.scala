/**
  * Nook.scala - Nooks
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.checker

import orchard.core.cell._



// //============================================================================================
// // NOOKS
// //

// abstract class AbstractNook[A : ExpressionLike] {

//   val isExpressionLike = implicitly[ExpressionLike[A]]
//   type ExpressionType = isExpressionLike.NonEmptyType

//   assert(framework.topCell.isExposedNook)

//   def framework : Framework[A]

//   val ncell : NCell[A] = framework.topCell.toNCell



// }

// class Nook(val framework : Framework[Option[Expression]]) extends AbstractNook[Option[Expression]] {

//   def map(f : Expression => Expression) : Nook = {
//     val duplicate = framework.duplicate

//     duplicate forAllCells (cell => {
//       cell.item = cell.item map f
//     })

//     new Nook(duplicate)
//   }

//   def withFiller(filler : Filler) : NCell[Expression] =
//     withFillerAndBoundary(filler, filler.Boundary)

//   def withFillerAndBoundary(filler : Expression, boundary : Expression) : NCell[Expression] = {
//     val frameworkCopy = framework.duplicate
//     frameworkCopy.topCell.item = Some(filler)
//     frameworkCopy.topCell.boundaryFace.item = Some(boundary)
//     frameworkCopy.topCell.toNCell map (_.get)
//   }
// }

