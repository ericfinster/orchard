/**
  * NewFramework.scala - A new checker based framework implementation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.checker

import orchard.core.cell._
import orchard.core.util._
import orchard.core.complex._

import scalaz._
import scalaz.std.vector._

import ErrorM._

trait NewFrameworkTrait { thisChecker : Checker =>

  object RoseTreeSequence {

    def sequence[A, B, M[+_]](tree : RoseTree[M[A], B])(implicit m : Monad[M]) : M[RoseTree[A, B]] =  {
      import m.monadSyntax._

      tree match {
        case Rose(b) => point(Rose(b))
        case Branch(ma, branches) => {
          val T = Traverse[Vector]

          for {
            a <- ma
            newBranches <- T.sequence(branches map (sequence(_)))
          } yield Branch(a, newBranches)
        }
      }
    }

  }

  trait FrameworkEntry {
    def isEmpty : Boolean
    def isThin : CheckerM[Boolean]
  }

  trait NewFramework[A <: FrameworkEntry] extends MutableSkeletalComplex[A] { 

    type FrameworkType <: NewFramework[A]
    type CellType <: NewFrameworkCell

    def emptyItem : A

    def extract(cell : CellType) : FrameworkType
    def duplicate : FrameworkType = extract(topCell)

    trait NewFrameworkCell extends MutableSkeletalCell { thisCell : CellType =>

      def isThin : CheckerM[Boolean] = item.isThin

      def isEmpty : Boolean = item.isEmpty
      def isFull : Boolean = ! isEmpty

      def isShell : Boolean = isEmpty && isCompleteShell
      def isComplete : Boolean = isFull && isCompleteShell

      def isCompleteShell : Boolean = 
        skeleton forall (face => (face != thisCell) || face.isFull)

      def isNook : Boolean = isInNook || isOutNook

      def isInNook : Boolean = 
        (for {
          srcs <- sources
          tgt <- target
        } yield {
          isEmpty && tgt.isComplete && (emptySources.length == 1) && 
          (1 + completeSources.length == sourceCount)
        }) getOrElse false


      def isOutNook : Boolean =
        (for {
          tgt <- target
        } yield {
          if (sourceCount == 0) {
            isEmpty && tgt.isShell
          } else {
            isEmpty && tgt.isEmpty && (sourceVector forall (_.isComplete))
          }
        }) getOrElse false


      // Okay, I'm pretty sure this is wildly inefficient and could be done much faster. Please
      // have a look at it again before deleting this comment. :)
      def isExposedNook : CheckerM[Boolean] = {
        if (isOutNook) checkerSucceed(true) else {
          if (isInNook) {

            val framework = extract(thisCell)
            val frameworkTgt = framework.topCell.target.get
            val emptyPtr = (new RoseZipper(frameworkTgt.canopy.get, Nil)).find(_.isEmpty).get

            def getDerivedOutNook(cell : framework.CellType) : NewFramework[A] = {
              val derivedFramework = framework.extract(cell)
              derivedFramework.topCell.item = emptyItem
              derivedFramework.topCell.target.get.item = emptyItem
              derivedFramework
            }

            // def checkBranchList(branches : Vector[RoseTree[framework.CellType, Int]]) : Unit = 
            //   branches foreach (_.foreachCell((cell : framework.CellType) => { status &&= getDerivedOutNook(cell).topCell.isExposedNook }))

              // branches forall (_.forallCells(getDerivedOutNook(_).topCell.isExposedNook))

            // var status : Boolean = true
            //((cell : framework.CellType) => getDerivedOutNook(cell).topCell.isExposedNook)

            // {
            //   branches foreach
            //   (branch => {
            //     branch foreachCell
            //     (cell => {
            //       status &&= getDerivedOutNook(cell).topCell.isExposedNook
            //     })
            //   })
            // }


            // // So, now we've got the right tree.
            // val descendantsOk = 
            //   emptyPtr.focus match {
            //     case Branch(_, branches) => checkBranchList(branches)
            //     case _ => checkerFail("Internal error: empty pointer is not a branch")
            //   }


            // def checkAtPointer(ptr : RoseZipper[framework.CellType, Int]) : CheckerM[Boolean] = 
            //   ptr.context match {
            //   }

      //       // First step is done ...
      //       if (! status) return false

      //       var ptr : RoseZipper[framework.CellType, Int] = emptyPtr

      //       while (ptr.context != Nil && status) {
      //         ptr = ptr.context match {
      //           case RoseContext(value, left, right) :: cs => {

      //             val incomingFace = ptr.focus.rootElement.get.target.get

      //             val faceSave = incomingFace.item
      //             val sourceSave = value.item

      //             incomingFace.item = implicitly[ExpressionLike[A]].empty
      //             value.item = implicitly[ExpressionLike[A]].empty

      //             val derivedNook = framework.extract(value)
      //             status &&= derivedNook.topCell.isExposedNook

      //             incomingFace.item = faceSave
      //             value.item = sourceSave

      //             // Now work on the left and right lists ...
      //             checkBranchList(left)
      //             checkBranchList(right)

      //             RoseZipper(Branch(value, left ++ List(ptr.focus) ++ right), cs)
      //           }
      //           case _ => throw new IllegalArgumentException
      //         }
      //       }

      //       status && (true /: (fullSources map (_.isThin))) (_ && _)

            ???
          } else { checkerSucceed(false) }
        }
      }

      //
      // Source and target collection
      //

      def sourceVector : Vector[CellType] = 
        sources getOrElse Vector.empty

      def targetVector : Vector[CellType] = 
        target map (Vector(_)) getOrElse Vector.empty

      def emptySources : Vector[CellType] = sourceVector filter (_.isEmpty)
      def fullSources : Vector[CellType] = sourceVector filter (_.isFull)
      def completeSources : Vector[CellType] = sourceVector filter (_.isComplete)

      def fullFaces : Vector[CellType] =
        (targetVector filter (_.isFull)) ++ fullSources

      //
      // The following couple methods should only be called on exposed nooks
      //

      // def isThinBoundary : Boolean = {
      //   val thinByInvertibility =
      //     invertibilityLevel match {
      //       case None => false
      //       case Some(l) => (dimension - 1) > l
      //     }

      //   if (isOutNook) {
      //     (true /: (sources.get map (_.isThin))) (_&&_) || thinByInvertibility
      //   } else {
      //     target.get.isThin || thinByInvertibility
      //   }
      // }

      // def boundaryFace : CellType =
      //   if (target.get.isEmpty)
      //     target.get
      //   else
      //     emptySources.head

      // def boundaryAddress : CellAddress =
      //   boundaryFace.address

    }

  }


  //============================================================================================
  // A SIMPLE IMPLEMENTATION
  //

  sealed trait SimpleEntry extends FrameworkEntry

  case object Empty extends SimpleEntry {
    def isEmpty = true
    def isThin = checkerFail("Thin request for empty cell.")
  }

  case class Full(expression : Expression) extends SimpleEntry {
    def isEmpty = false
    def isThin = expression.isThin
  }

  class SimpleFramework(seed : NCell[SimpleEntry]) extends AbstractComplex[SimpleEntry](seed) {

    type CellType = SimpleFrameworkCell

    def newCell(item : SimpleEntry) = new SimpleFrameworkCell(item)

    class SimpleFrameworkCell(var item : SimpleEntry) extends AbstractComplexCell {


    }

  }

}
