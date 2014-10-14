/**
  * Render.scala - Rendering Routines
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.tree

import scalaz.{Tree => _, Zipper => _, _}
import scalaz.std.option._

import Nats._
import Tree._

trait Sizeable[A] {

  def widthOf(a : A) : Double
  def heightOf(a : A) : Double

}

object Sizeable {

  implicit class SizeableOps[A : Sizeable](a : A) {

    def width : Double = implicitly[Sizeable[A]].widthOf(a)
    def height : Double = implicitly[Sizeable[A]].heightOf(a)

  }

}

trait RenderData {

  //=================================
  // RENDER OPTIONS
  //

  def strokeWidth : Double
  def internalPadding : Double
  def externalPadding : Double

  //=================================
  // POSITIONING
  //

  def x : Double = rootX - rootLeftMargin
  def y : Double = rootY - height

  def rootX : Double
  def rootY : Double

  //=================================
  // SIZING
  //

  def width : Double = 
    rootLeftMargin + rootRightMargin

  def height : Double =
    strokeWidth +
      internalPadding +
      internalHeight +
      labelHeight +
      internalPadding +
      strokeWidth

  def internalWidth : Double
  def internalHeight : Double 

  def rootLeftMargin : Double
  def rootRightMargin : Double 

  //=================================
  // LABEL DATA
  //

  def labelX : Double
  def labelY : Double

  def labelWidth : Double
  def labelHeight : Double

  def labelPadding : Double

}

abstract class Renderer { thisRenderer =>

  import Sizeable._

  //============================================================================================
  // VISUAL OPTIONS
  //

  def arcRadius : Double 

  def internalPadding : Double 
  def externalPadding : Double 

  def halfLeafWidth : Double 
  def halfStrokeWidth : Double

  def strokeWidth = halfStrokeWidth + halfStrokeWidth
  def leafWidth = halfLeafWidth + strokeWidth + halfLeafWidth

  //============================================================================================
  // OBJECT NESTING RENDERING
  //

  def renderObjectNesting[A : Sizeable](nst : Nesting[_0, A]) : Nesting[_0, RenderData] =
    nst match {
      case Obj(a) => ???
      case Box(a, c) => ???
    }

  //============================================================================================
  // POSITIVE DIMENSIONAL NESTING RENDERING
  //

  def renderProperNesting[N <: Nat, A : Sizeable](
    nst : Nesting[S[N], A], 
    lvs : Tree[N, LayoutMarker]
  ) : Option[(LayoutMarker, Nesting[S[N], RenderData])] =
    nst match {
      case Dot(a, c) => {

        val dotData = new RenderMarker

        dotData.labelWidth = a.width
        dotData.labelHeight = a.height

        dotData.rootLeftMargin =
          strokeWidth +
            internalPadding +
            (dotData.labelWidth / 2)

        dotData.rootRightMargin =
          (dotData.labelWidth / 2) +
            internalPadding +
            strokeWidth

        val leafMarkers = lvs.nodes
        val leafCount = leafMarkers.length

        if (leafCount == 0) {
          // This is a drop

          ???
        } else {

          def layoutLeft(markers : List[LayoutMarker]) : Unit = {
            for (i <- Range(markers.length - 2, -1, -1)) {
              val lastMarker = markers(i + 1)
              val thisMarker = markers(i)

              thisMarker.owner
                .horizontalShift(lastMarker.owner.rootX -
                  lastMarker.leftMargin -
                  externalPadding -
                  thisMarker.rightMargin -
                  thisMarker.owner.rootX)

              dotData.horizontalDependents :+= thisMarker.owner
            }
          }

          def layoutRight(markers : List[LayoutMarker]) : Unit = {
            for (i <- Range(1, markers.length)) {
              val lastMarker = markers(i-1)
              val thisMarker = markers(i)

              thisMarker.owner
                .horizontalShift(lastMarker.owner.rootX +
                  lastMarker.rightMargin +
                  externalPadding +
                  thisMarker.leftMargin -
                  thisMarker.owner.rootX)

              dotData.horizontalDependents :+= thisMarker.owner
            }
          }

          val isOdd = (leafCount & 1) != 0

          val leftChildren = 
            leafMarkers.slice(0, leafCount / 2)

          val rightChildren =
            if (isOdd)
              leafMarkers.slice(leafCount / 2 + 1, leafCount)
            else
              leafMarkers.slice(leafCount / 2, leafCount)

          val midChild = leafMarkers(leafCount / 2)

          ???
        }
      }
      case Box(a, c) => {

        val (leafCount, leavesWithIndices) = lvs.zipWithIndex

        def verticalPass(tr : Tree[S[N], Nesting[S[N], A]]) : Option[(LayoutMarker, Tree[S[N], Nesting[S[N], RenderData]])] =
          tr match {

            case l @ Leaf(addr) => 
              for {
                leafMarkerWithIndex <- leavesWithIndices valueAt addr
              } yield {

                val (leafMarker, leafIndex) = leafMarkerWithIndex

                if (leafIndex == 0 && leafCount == 1) {
                  (leafMarker.truncateUnique, Leaf(addr)(l.p))
                } else if (leafIndex == 0) {
                  (leafMarker.truncateLeft, Leaf(addr)(l.p))
                } else if (leafIndex == leafCount - 1) {
                  (leafMarker.truncateRight, Leaf(addr)(l.p))
                } else {
                  (leafMarker.truncateMiddle, Leaf(addr)(l.p))
                }

              }

            case Node(sn, vns) => 
              for {
                vresult <- vns traverse (verticalPass(_))
                (layoutTree, resultTree) = unzip(vresult)
                lresult <- renderProperNesting(sn, layoutTree)
                (localLayout, resultNesting) = lresult
              } yield (localLayout, Node(resultNesting, resultTree)) // And here you need to do more arrangement/layout

          }

        val boxData = new RenderMarker

        boxData.labelWidth = a.width
        boxData.labelHeight = a.height

        for {
          internalResult <- verticalPass(c)
        } yield ()

        ???

      }
    }

  //============================================================================================
  // MARKER HELPER CLASSES
  //

  protected class RenderMarker extends RenderData {

    val strokeWidth : Double = thisRenderer.strokeWidth
    val internalPadding : Double = thisRenderer.internalPadding
    val externalPadding : Double = thisRenderer.externalPadding

    var rootX : Double = 0.0
    var rootY : Double = 0.0

    var internalWidth : Double = 0.0
    var internalHeight : Double = 0.0

    var rootLeftMargin : Double = 0.0
    var rootRightMargin : Double = 0.0

    var labelX : Double = 0.0
    var labelY : Double = 0.0

    var labelWidth : Double = 0.0
    var labelHeight : Double = 0.0

    var labelPadding : Double = 0.0

    var horizontalDependents : List[RenderMarker] = Nil
    var verticalDependents : List[RenderMarker] = Nil

    def horizontalShift(amount : Double) : Unit = {
      if (amount != 0) {
        rootX += amount
        horizontalDependents foreach (_.horizontalShift(amount))
      }
    }

    def verticalShift(amount : Double) : Unit = {
      if (amount != 0) {
        rootY += amount
        verticalDependents foreach (_.verticalShift(amount))
      }
    }

  }


  abstract class LayoutMarker { thisMarker =>

    val owner : RenderMarker
    val wasExternal : Boolean

    def height : Double = 0.0

    def leftSubtreeMargin : Double = 0.0
    def rightSubtreeMargin : Double = 0.0
    def leftInternalMargin : Double = 0.0
    def rightInternalMargin : Double = 0.0

    def leftMargin : Double =
      leftSubtreeMargin + leftInternalMargin + halfLeafWidth + halfStrokeWidth

    def rightMargin : Double =
      rightSubtreeMargin + rightInternalMargin + halfLeafWidth + halfStrokeWidth

    def width : Double =
      leftMargin + rightMargin

    // Truncations

    def truncateLeft : LayoutMarker =
      new LayoutMarker {
        val owner = thisMarker.owner
        val wasExternal = true
        override def rightSubtreeMargin = thisMarker.rightSubtreeMargin
        override def rightInternalMargin = thisMarker.rightInternalMargin
      }

    def truncateRight : LayoutMarker =
      new LayoutMarker {
        val owner = thisMarker.owner
        val wasExternal = true
        override def leftSubtreeMargin = thisMarker.leftSubtreeMargin
        override def leftInternalMargin = thisMarker.leftInternalMargin
      }

    def truncateUnique : LayoutMarker =
      new LayoutMarker {
        val owner = thisMarker.owner
        val wasExternal = true
      }

    def truncateMiddle : LayoutMarker =
      new LayoutMarker {
        val owner = thisMarker.owner
        val wasExternal = true
        override def leftSubtreeMargin = thisMarker.leftSubtreeMargin
        override def rightSubtreeMargin = thisMarker.rightSubtreeMargin
        override def leftInternalMargin = thisMarker.leftInternalMargin
        override def rightInternalMargin = thisMarker.rightInternalMargin
      }

    override def toString = "LM(" ++ owner.toString ++ ")" ++ 
      "(we = " ++ wasExternal.toString ++ ", ht = " ++ height.toString ++
      ", lsm = " ++ leftSubtreeMargin.toString ++
      ", lim = " ++ leftInternalMargin.toString ++
      ", rim = " ++ rightInternalMargin.toString ++
      ", rsm = " ++ rightSubtreeMargin.toString ++ ")"

  }

}
