/**
  * Frameworks.scala - Frameworks for the type checker
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
import scalaz.std.option._
import scalaz.syntax.traverse._

import ErrorM.{succeed => succeedE, fail => failE, _}
import MonadUtils._

trait Frameworks { thisChecker : TypeChecker =>

  import CheckerErrorSyntax._

  trait CellExpressionContainer[A] {

    def empty : A

    def expression(a : A) : Error[CellExpression]

    def isEmpty(a : A) : Boolean = a == empty
    def isThin(a : A) : Boolean 

  }

  object CellExpressionContainer {

    implicit class ContainerOps[A : CellExpressionContainer](container : A) {

      val ev = implicitly[CellExpressionContainer[A]]

      def isEmpty = ev.isEmpty(container)
      def isThin = ev.isThin(container)
      def expression = ev.expression(container)

    }

  }

  abstract class AbstractFramework[A : CellExpressionContainer](seed : NCell[A]) extends AbstractComplex[A](seed) {

    type FrameworkType <: AbstractFramework[A]
    type CellType <: AbstractFrameworkCell

    import CellExpressionContainer._

    def emptyItem : A = implicitly[CellExpressionContainer[A]].empty

    // This should be better typed to allow extraction to other framework types
    // In particular, worksheets should extract to frameworks ....
    def extract(cell : CellType) : FrameworkType
    def duplicate : FrameworkType = extract(topCell)

    def isShell = topCell.isShell
    def isExposedNook = topCell.isExposedNook

    trait AbstractFrameworkCell extends AbstractComplexCell { thisCell : CellType =>

      def isThin : Boolean = item.isThin

      def isEmpty : Boolean = item.isEmpty
      def isFull : Boolean = ! isEmpty

      def isShell : Boolean = isEmpty && isCompleteShell
      def isComplete : Boolean = isFull && isCompleteShell

      def isCompleteShell : Boolean = 
        skeleton forall (face => (face == thisCell) || face.isFull)

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
      def isExposedNook : Boolean = {
        if (isOutNook) true else {
          if (isInNook) {

            // In order to be efficient and be able to cancel the operation, I'd like
            // to extend the checker monad here with an option.  Let's see if that works.

            type Cancellable[A] = Option[A]

            def stopOnFalse(cond : Boolean) : Cancellable[Unit] = 
              if (cond) Some(()) else None

            val framework = extract(thisCell)
            val frameworkTgt = framework.topCell.target.get
            val emptyPtr = (new RoseZipper(frameworkTgt.canopy.get, Nil)).find(_.isEmpty).get

            def getDerivedOutNook(cell : framework.CellType) : AbstractFramework[A] = {
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
                case _ => stopOnFalse(false) //fail("Internal error: empty pointer is not a branch"))
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
                case Nil => stopOnFalse(true)
              }
            }

            val exposedNookTest = 
              for {
                _ <- (fullSources map ((cell : CellType) => stopOnFalse(cell.isThin))).sequence
                _ <- descendantCheck
                _ <- checkFromPointer(emptyPtr)
              } yield ()

            exposedNookTest.isDefined

          } else { false }
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

      def isThinBoundary : Boolean = {
        if (isOutNook) {
          sourceVector forall (_.isThin)
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

      def expression : Error[CellExpression] = item.expression

      def bindingSkeleton : Error[NCell[Either[CellAddress, CellExpression]]] = {
        NCell.sequence[Either[CellAddress, CellExpression], Error](
          skeleton map (cell =>
            if (cell.item.isEmpty)
              succeedE(Left(cell.address))
            else
              for {
                expr <- cell.item.expression
              } yield Right(expr)
          )
        )
      }
    }
  }

  //============================================================================================
  // A SIMPLE FRAMEWORK IMPLEMENTATION
  //

  sealed trait FrameworkEntry

  case object Empty extends FrameworkEntry
  case class Full(val expression : CellExpression) extends FrameworkEntry 

  object FrameworkEntry {

    implicit val frameworkEntryIsContainer : CellExpressionContainer[FrameworkEntry] =
      new CellExpressionContainer[FrameworkEntry] {

        def empty : FrameworkEntry = Empty

        def expression(entry : FrameworkEntry) : Error[CellExpression] =
          entry match {
            case Empty => failE("Empty container has no expression")
            case Full(expr) => succeedE(expr)
          }

        def isThin(entry : FrameworkEntry) : Boolean = 
          (expression(entry) map (_.isThin)) getOrElse false

      }

  }

  class Framework(seed : NCell[FrameworkEntry]) extends AbstractFramework[FrameworkEntry](seed) {

    type CellType = FrameworkCell
    type FrameworkType = Framework

    def newCell(item : FrameworkEntry) = new FrameworkCell(item)
    def extract(cell : FrameworkCell) = new Framework(cell.skeleton map (_.item))

    class FrameworkCell(var item : FrameworkEntry) extends AbstractFrameworkCell

  }

  //============================================================================================
  // SHELLS AND NOOKS
  //

  class Nook(val ncell : NCell[FrameworkEntry]) {

    val framework = new Framework(ncell)

    assert(framework.isExposedNook)

    def isThinBoundary : Boolean =
      framework.topCell.isThinBoundary

    def withFiller(filler : Filler) : NCell[CellExpression] =
      withFillerAndBoundary(filler, filler.Boundary)

    def withBoundary(bdry : Filler#BoundaryExpr) : NCell[CellExpression] = 
      framework.topCell.boundaryFace.skeleton map (cell => 
        cell.item match {
          case Empty => bdry
          case Full(e) => e
        }
      )

    def withFillerAndBoundary(filler : CellExpression, boundary : CellExpression) : NCell[CellExpression] =
      (ncell match {
        case Composite(_, srcTree, tgtValue, ev) =>
          Composite(Full(filler), srcTree, tgtValue)
      }) map {
        case Empty => boundary
        case Full(e) => e
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

    override def toString = ncell.toString

  }

  object Nook {

    def apply(ncell : NCell[FrameworkEntry]) : Error[Nook] = 
      try {
        val nook = new Nook(ncell)
        succeedE(nook)
      } catch {
        // What's the right thing to catch here????
        case e : Exception => failE("Framework is not an exposed nook")
      }

  }

  class Shell(val ncell : NCell[FrameworkEntry]) {

    val framework = new Framework(ncell)

    assert(framework.isShell)

    def withFillingExpression(expr : CellExpression) : NCell[CellExpression] =
      ncell map {
        case Empty => expr
        case Full(e) => e
      }

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

  object Shell {

    def apply(ncell : NCell[FrameworkEntry]) : Error[Shell] = 
      try {
        val shell = new Shell(ncell)
        succeedE(shell)
      } catch {
        // What's the right thing to catch here????
        case e : Exception => failE("Framework is not a shell")
      }

  }

}
