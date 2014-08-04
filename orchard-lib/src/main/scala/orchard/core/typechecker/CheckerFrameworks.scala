/**
  * CheckerFrameworks.scala - Frameworks for the type checker
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.typechecker

import orchard.core.cell._
import orchard.core.util._
import orchard.core.complex._

import scalaz._
import scalaz.std.vector._
import scalaz.syntax.traverse._

import ErrorM._
import MonadUtils._

trait CheckerFrameworks { thisChecker : Checker =>

  trait FrameworkEntry {

    def isEmpty : Boolean
    def isThin : CheckerM[Boolean]

    def expression : Expression  // May throw an exception

  }

  trait Framework[A <: FrameworkEntry] extends MutableSkeletalComplex[A] { 

    type FrameworkType <: Framework[A]
    type CellType <: FrameworkCell

    def emptyItem : A

    def extract(cell : CellType) : FrameworkType
    def duplicate : FrameworkType = extract(topCell)

    trait FrameworkCell extends MutableSkeletalCell { thisCell : CellType =>

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

            // In order to be efficient and be able to cancel the operation, I'd like
            // to extend the checker monad here with an option.  Let's see if that works.

            type Cancellable[A] = OptionT[CheckerM, A]

            val CM = Monad[Cancellable]

            def stopOnFalse(cm : CheckerM[Boolean]) : Cancellable[Unit] = 
              OptionT[CheckerM, Unit](cm map {
                case true => Some(())
                case false => None
              })

            val framework = extract(thisCell)
            val frameworkTgt = framework.topCell.target.get
            val emptyPtr = (new RoseZipper(frameworkTgt.canopy.get, Nil)).find(_.isEmpty).get

            def getDerivedOutNook(cell : framework.CellType) : Framework[A] = {
              val derivedFramework = framework.extract(cell)
              derivedFramework.topCell.item = emptyItem
              derivedFramework.topCell.target.get.item = emptyItem
              derivedFramework
            }

            def checkSubtreeDerivedOutNooks(tree : RoseTree[framework.CellType, Int]) : Cancellable[Unit] = {

              // Question:  Why copy it?  Couldn't you modify the framework in place and call 
              // the funtion?

              type IntRose[A] = RoseTree[A, Int]
              val T = Traverse[IntRose]

              for {
                _ <- T.sequence(
                  tree mapCells ((cell : framework.CellType) =>
                    stopOnFalse(getDerivedOutNook(cell).topCell.isExposedNook)
                  )
                )
              } yield ()
            }

            def checkBranchList(branches : Vector[RoseTree[framework.CellType, Int]]) : Cancellable[Unit] =
              for {
                _ <- (branches map (checkSubtreeDerivedOutNooks(_))).sequence
              } yield ()

            val descendantCheck : Cancellable[Unit] = 
              emptyPtr.focus match {
                case Branch(_, branches) => checkBranchList(branches)
                case _ => stopOnFalse(checkerFail("Internal error: empty pointer is not a branch"))
              }

            def checkFromPointer(ptr : RoseZipper[framework.CellType, Int]) : Cancellable[Unit] = {
              ptr.context match {
                case RoseContext(value, left, right) :: cs => {

                  val incomingFace = ptr.focus.rootElement.get.target.get

                  val faceSave = incomingFace.item
                  val sourceSave = value.item

                  incomingFace.item = emptyItem
                  value.item = emptyItem

                  val derivedNook = framework.extract(value)

                  incomingFace.item = faceSave
                  value.item = sourceSave

                  for {
                    _ <- checkFromPointer(
                      RoseZipper(Branch(value, left ++ Vector(ptr.focus) ++ right), cs)
                    )
                    _ <- stopOnFalse(derivedNook.topCell.isExposedNook)
                    _ <- checkBranchList(left)
                    _ <- checkBranchList(right)
                  } yield ()

                }
                case Nil => stopOnFalse(checkerSucceed(true))
              }
            }

            val exposedNookTest = 
              for {
                _ <- (fullSources map ((cell : CellType) => stopOnFalse(cell.isThin))).sequence
                _ <- descendantCheck
                _ <- checkFromPointer(emptyPtr)
              } yield ()

            exposedNookTest.isDefined

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

      def isThinBoundary : CheckerM[Boolean] = {
        if (isOutNook) {
          for {
            sourceResults <- (sourceVector map (_.isThin)).sequence
          } yield {
            sourceResults forall (b => b)
          }
        } else {
          target.get.isThin  
        }
      }

      def boundaryFace : CellType =
        if (target.get.isEmpty)
          target.get
        else
          emptySources.head

      def boundaryAddress : CellAddress =
        boundaryFace.address


      def bindingSkeleton : NCell[Either[CellAddress, Expression]] =
        skeleton map (cell =>
          if (cell.item.isEmpty) 
            Left(cell.address) 
          else 
            Right(cell.item.expression)
        )

    }

  }


  //============================================================================================
  // A SIMPLE IMPLEMENTATION
  //

  sealed trait SimpleEntry extends FrameworkEntry 

  case object Empty extends SimpleEntry {
    def isEmpty = true
    def isThin = checkerFail("Thin request for empty cell.")
    def expression = throw new Exception("Empty cell has no expression")
  }

  case class Full(val expression : Expression) extends SimpleEntry {
    def isEmpty = false
    def isThin = expression.isThin
  }

  class SimpleFramework(seed : NCell[SimpleEntry]) extends AbstractComplex[SimpleEntry](seed) with Framework[SimpleEntry] {

    type CellType = SimpleFrameworkCell
    type FrameworkType = SimpleFramework

    def newCell(item : SimpleEntry) = new SimpleFrameworkCell(item)
    def extract(cell : SimpleFrameworkCell) = new SimpleFramework(cell.skeleton map (_.item))

    def emptyItem : SimpleEntry = Empty

    class SimpleFrameworkCell(var item : SimpleEntry) extends AbstractComplexCell with FrameworkCell

  }

  //============================================================================================
  // SHELLS AND NOOKS
  //


  class Nook(val ncell : NCell[SimpleEntry]) {

    val framework = new SimpleFramework(ncell)

    def isThinBoundary : CheckerM[Boolean] =
      framework.topCell.isThinBoundary

    def withFiller(filler : Filler) : NCell[Expression] =
      withFillerAndBoundary(filler, filler.Boundary)

    def withFillerAndBoundary(filler : Expression, boundary : Expression) : NCell[Expression] = {
      val frameworkCopy = framework.duplicate
      frameworkCopy.topCell.item = Full(filler)
      frameworkCopy.topCell.boundaryFace.item = Full(boundary)
      frameworkCopy.topCell.toNCell map (_.expression)
    }

    def canEqual(other : Any) : Boolean =
      other.isInstanceOf[Nook]

    override def equals(other : Any) : Boolean =
      other match {
        case that : Nook =>
          (that canEqual this) && (that.ncell == this.ncell)
        case _ => false
      }

    override def hashCode : Int =
      41 * (41 + ncell.hashCode)

  }

  class Shell(val ncell : NCell[SimpleEntry]) {

    val framework = new SimpleFramework(ncell)

    def withFillingExpression(expr : Expression) : NCell[Expression] =
      framework.topCell.skeleton map (cell => {
        cell.item match {
          case Empty => expr
          case Full(e) => e
        }
      })

    def canEqual(other : Any) : Boolean =
      other.isInstanceOf[Shell]

    override def equals(other : Any) : Boolean =
      other match {
        case that : Shell =>
          (that canEqual this) && (that.ncell == this.ncell)
        case _ => false
      }

    override def hashCode : Int =
      41 * (41 + ncell.hashCode)

  }

}
