/**
  * CheckableFramework.scala - A Framework with checkable filling parameters
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

trait HasFillingParameters {

  def stabilityLevel : Option[Int]
  def invertibilityLevel : Option[Int]
  def unicityLevel : Option[Int]

}

trait CheckableEnvironment extends HasFillingParameters {

  trait CheckableFramework[A] { thisFramework : Framework[A] =>

    trait CheckableCell { thisCell : FrameworkCell =>

      def isUnicityFillable : Boolean =
        unicityLevel match {
          case None => false
          case Some(l) => dimension > l
        }

      def isFillable : Boolean =
        if (isUnicityFillable) true else isExposedNook

      // For an exposed nook, determine if the filler face is thin
      def isThinFillerFace : Boolean = {
        val thinByInvertibility =
          invertibilityLevel match {
            case None => false
            case Some(l) => (dimension - 1) > l
          }

        if (isOutNook) {
          (true /: (sources.get map (_.isThin))) (_&&_) || thinByInvertibility
        } else {
          target.get.isThin || thinByInvertibility
        }
      }

    }
  }

}

// abstract class CheckableFramework[A : FrameworkInhabitant](seed : NCell[A])
//     extends Framework[A](seed) { thisFramework : HasFillingParameters =>

//   type CellType <: CheckableFrameworkCell

//   abstract class CheckableFrameworkCell(itm : A) extends FrameworkCell(itm) { thisCell : CellType =>

//     def isUnicityFillable : Boolean =
//       unicityLevel match {
//         case None => false
//         case Some(l) => dimension > l
//       }

//     def isFillable : Boolean =
//       if (isUnicityFillable) true else isExposedNook

//     // For an exposed nook, determine if the filler face is thin
//     def isThinFillerFace : Boolean = {
//       val thinByInvertibility =
//         invertibilityLevel match {
//           case None => false
//           case Some(l) => (dimension - 1) > l
//         }

//       if (isOutNook) {
//         (true /: (sources.get map (_.isThin))) (_&&_) || thinByInvertibility
//       } else {
//         target.get.isThin || thinByInvertibility
//       }
//     }

//   }

// }
