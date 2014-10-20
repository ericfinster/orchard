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

trait BBox {

  def width : Double
  def height : Double

}

trait RenderOptions {

  def arcRadius : Double 

  def internalPadding : Double 
  def externalPadding : Double 

  def halfLeafWidth : Double 
  def halfStrokeWidth : Double

  def strokeWidth = halfStrokeWidth + halfStrokeWidth
  def leafWidth = halfLeafWidth + strokeWidth + halfLeafWidth

}

trait RenderData extends RenderOptions {

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

  def labelWidth : Double
  def labelHeight : Double

  def labelPadding : Double

  def labelX : Double = rootX + halfStrokeWidth + halfLeafWidth
  def labelY : Double = rootY + strokeWidth + internalPadding

}

abstract class Renderer[A] extends RenderOptions { thisRenderer =>

  trait VisualElement {

    def x : Double
    def y : Double

    def width : Double
    def height : Double

  }

  //============================================================================================
  // TYPE DEFINITIONS
  //

  // type BoxElement <: VisualElement
  // type DotElement <: BoxElement with EdgeInfo
  // type EdgeElement

  //============================================================================================
  // ABSTRACT MEMBERS
  //

  def getLabelBBox(a : A) : BBox

  // def renderDot(a : A) : DotElement
  // def renderBox(a : A, internalLayout : LayoutMarker) : BoxElement

  // def createEdge : EdgeElement

  //============================================================================================
  // COMPLEX RENDERING
  //

  type RenderIn[M <: Nat] = Complex[M, A]
  type RenderOut[M <: Nat] = Option[Complex[M, RenderData]]

  object RenderRecursor extends NatRecursor[RenderIn, RenderOut] {

    def caseZero(zc : Complex[_0, A]) : Option[Complex[_0, RenderData]] =
      for {
        objResult <- renderObjectNesting(zc.head)
      } yield Base(objResult._2)

    def caseSucc[P <: Nat](sc : Complex[S[P], A]) : Option[Complex[S[P], RenderData]] =
      sc match {
        case Append(tl, hd) =>
          for {
            tailResult <- renderComplex(tl)
            leaves <- tailResult.head.spine 
            headResult <- renderNesting(hd, leaves map (_.asInstanceOf[RenderMarker].edgeMarker))  // The cast here is annoying ...
          } yield {
            Append(tailResult, headResult._2)
          }
      }

  }

  def renderComplex[N <: Nat](cmplx : Complex[N, A]) : Option[Complex[N, RenderData]] =
    natRec(cmplx.dim)(RenderRecursor)(cmplx)

  //============================================================================================
  // OBJECT NESTING RENDERING
  //

  def renderObjectNesting(nst : Nesting[_0, A]) : Option[(LayoutMarker, Nesting[_0, RenderData])] = 
    nst match {

      case Obj(a) => {

        val objData = new RenderMarker
        val labelBBox = getLabelBBox(a)

        objData.labelWidth = labelBBox.width
        objData.labelHeight = labelBBox.height

        objData.rootLeftMargin = strokeWidth + internalPadding + (objData.labelWidth / 2)
        objData.rootRightMargin = (objData.labelWidth / 2) + internalPadding + strokeWidth

        val marker = new LayoutMarker {

          val owner = objData
          val wasExternal = false

          override def height = objData.height

          override def leftInternalMargin = objData.rootLeftMargin - halfLeafWidth - halfStrokeWidth
          override def rightInternalMargin = objData.rootRightMargin - halfLeafWidth - halfStrokeWidth

        }

        Some(marker, Obj(objData))

      }

      case Box(a, Pt(c)) => 
        for {
          interior <- renderObjectNesting(c)
        } yield {

          val (interiorLayout, interiorNesting) = interior

          val boxData = new RenderMarker
          val labelBBox = getLabelBBox(a)

          boxData.labelWidth = labelBBox.width
          boxData.labelHeight = labelBBox.height

          boxData.internalWidth = interiorLayout.width
          boxData.internalHeight = interiorLayout.height

          boxData.rootLeftMargin = interiorLayout.leftMargin + internalPadding + strokeWidth
          boxData.rootRightMargin = interiorLayout.rightMargin + internalPadding + strokeWidth

          boxData.labelPadding = 0.0

          val marker = new LayoutMarker {

            val owner = boxData
            val wasExternal = false

            override def height = boxData.height

            override def leftInternalMargin = boxData.rootLeftMargin - halfLeafWidth - halfStrokeWidth
            override def rightInternalMargin = boxData.rootRightMargin - halfLeafWidth - halfStrokeWidth

          }

          (marker, Box(boxData, Pt(interiorNesting)))

        }
    }

  //============================================================================================
  // POSITIVE DIMENSIONAL NESTING RENDERING
  //

  def renderNesting[N <: Nat](nst : Nesting[S[N], A], lvs : Tree[N, LayoutMarker]) 
      : Option[(LayoutMarker, Nesting[S[N], RenderData])] =
    nst match {
      case Dot(a, c) => {

        val dotData = new RenderMarker
        val labelBBox = getLabelBBox(a)

        dotData.labelWidth = labelBBox.width
        dotData.labelHeight = labelBBox.height

        dotData.rootLeftMargin = strokeWidth + internalPadding + (dotData.labelWidth / 2)
        dotData.rootRightMargin = (dotData.labelWidth / 2) + internalPadding + strokeWidth

        val leafMarkers = lvs.nodes
        val leafCount = leafMarkers.length

        val marker = 
          if (leafCount == 0) {  // This is a drop. Simply return an appropriate marker ...

            new LayoutMarker {

              val owner = dotData
              val wasExternal = false

              override def height = dotData.height
              override def leftInternalMargin = dotData.rootLeftMargin - halfLeafWidth - halfStrokeWidth
              override def rightInternalMargin = dotData.rootRightMargin - halfLeafWidth - halfStrokeWidth

            }

          } else { // We have children.  Arrange them and calculate the marker.

            val isOdd = (leafCount & 1) != 0

            val firstMarker = leafMarkers.head
            val lastMarker = leafMarkers.last

            val midChild = leafMarkers(leafCount / 2)
            if (isOdd) dotData.horizontalDependants :+= midChild.owner

            if (leafCount > 1) {

              val leftChildren = leafMarkers.slice(0, leafCount / 2)
              val rightChildren = leafMarkers.slice(leafCount / 2 + (leafCount & 1), leafCount)

              val leftChild = leftChildren.last
              val rightChild = rightChildren.head

              val midLeftOffset = if (isOdd) midChild.leftMargin + externalPadding + leftChild.rightMargin else 0.0
              val midRightOffset = if (isOdd) midChild.rightMargin + externalPadding + rightChild.leftMargin else 0.0

              val leftChildShift : Double =
                Math.max(Math.max(midLeftOffset, leftChild.rightMargin + (externalPadding / 2)),
                  dotData.rootLeftMargin + halfLeafWidth + halfStrokeWidth + arcRadius) // Arc Radius here is a bit suspicious ...

              val rightChildShift : Double =
                Math.max(Math.max(midRightOffset, rightChild.leftMargin + (externalPadding / 2)),
                  dotData.rootRightMargin + halfLeafWidth + halfStrokeWidth + arcRadius)

              val leftEdge : Double =
                (leftChildren foldRight leftChildShift)({
                  case (currentMarker, leftShift) => {
                    val thisShift = leftShift + externalPadding + currentMarker.rightMargin
                    currentMarker.owner.shiftLeft(thisShift)
                    dotData.horizontalDependants :+= currentMarker.owner
                    thisShift + currentMarker.leftMargin
                  }
                })

              val rightEdge : Double =
                (rightChildren foldLeft rightChildShift)({
                  case (rightShift, currentMarker) => {
                    val thisShift = rightShift + externalPadding + currentMarker.leftMargin
                    currentMarker.owner.shiftRight(thisShift)
                    dotData.horizontalDependants :+= currentMarker.owner
                    thisShift + currentMarker.rightMargin
                  }
                })

            }

            val (maxLeafHeight, allExternal) : (Double, Boolean) =
              (leafMarkers map (m => (m.height, m.wasExternal)) foldLeft (0.0, true))({
                case ((x , b),(y, c)) => (Math.max(x, y), b && c)
              })

            new LayoutMarker {

              val owner = dotData
              val wasExternal = false

              override def height = maxLeafHeight + dotData.height + (if (allExternal) 0.0 else externalPadding)
              override def leftSubtreeMargin = firstMarker.leftMargin - halfLeafWidth - halfStrokeWidth
              override def rightSubtreeMargin = lastMarker.rightMargin - halfLeafWidth - halfStrokeWidth
              override def leftInternalMargin = - firstMarker.owner.rootX
              override def rightInternalMargin = lastMarker.owner.rootX

            }

          }

        Some(marker, Dot(dotData, c))

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
                  (leafMarker.truncateUnique, Leaf(addr))
                } else if (leafIndex == 0) {
                  (leafMarker.truncateLeft, Leaf(addr))
                } else if (leafIndex == leafCount - 1) {
                  (leafMarker.truncateRight, Leaf(addr))
                } else {
                  (leafMarker.truncateMiddle, Leaf(addr))
                }

              }

            case Node(sn, vns) => 
              for {
                vresult <- vns traverse (verticalPass(_))
                (layoutTree, resultTree) = unzip(vresult)
                lresult <- renderNesting(sn, layoutTree)
                (localLayout, resultNesting) = lresult
              } yield {

                // Indeed.  Here what we are going to do is fold over the children.  We do three things:
                //
                //  1) Set their vertical positions
                //  2) Find the leftmost and rightmost marker so that we can correctly setup the return maker
                //  3) Find the highest branch
                //

                val descendantMarkers : List[LayoutMarker] = layoutTree.nodes

                val (leftMostChild, rightMostChild, heightOfChildren) = 
                  (descendantMarkers foldLeft (localLayout, localLayout, 0.0))({
                    case ((lcMarker, rcMarker, ht), thisMarker) => {

                      if (! thisMarker.wasExternal) {
                        thisMarker.owner.shiftUp(localLayout.height)
                        localLayout.owner.verticalDependants :+= thisMarker.owner
                      }

                      val newLeftChild = if (thisMarker.leftEdge < lcMarker.leftEdge) thisMarker else lcMarker
                      val newRightChild = if (thisMarker.rightEdge < lcMarker.rightEdge) thisMarker else rcMarker

                      (newLeftChild, newRightChild, Math.max(ht, thisMarker.height))

                    }
                  })

                val marker = new LayoutMarker {

                  val owner = localLayout.owner
                  val wasExternal = false

                  override def height = localLayout.height + heightOfChildren

                  override def leftInternalMargin = - (leftMostChild.owner.rootX - leftMostChild.leftInternalMargin)
                  override def rightInternalMargin = rightMostChild.owner.rootX + rightMostChild.rightInternalMargin

                  override def leftSubtreeMargin = leftMostChild.leftSubtreeMargin
                  override def rightSubtreeMargin = rightMostChild.rightSubtreeMargin

                }

                (marker, Node(resultNesting, resultTree)) 

              }
          }

        for {
          internalResult <- verticalPass(c)
        } yield {

          val (rootMarker, canopy) = internalResult

          val boxData = new RenderMarker
          val labelBBox = getLabelBBox(a)

          boxData.labelWidth = labelBBox.width
          boxData.labelHeight = labelBBox.height

          boxData.internalWidth = rootMarker.width
          boxData.internalHeight = rootMarker.height

          // Shift the lable so that it is flush with the right edge
          boxData.labelPadding = Math.max(0.0, boxData.labelWidth - rootMarker.rightMargin) + internalPadding

          boxData.rootLeftMargin = strokeWidth + internalPadding + rootMarker.leftMargin
          boxData.rootRightMargin = rootMarker.rightMargin + boxData.labelPadding + internalPadding + strokeWidth

          // Set the dependencies
          boxData.horizontalDependants :+= rootMarker.owner

          // Setup and return an appropriate marker
          val marker = new LayoutMarker {

            val owner = boxData
            val wasExternal = false

            override def height = boxData.height
            override def leftInternalMargin = boxData.rootLeftMargin - halfLeafWidth - halfStrokeWidth
            override def rightInternalMargin = boxData.rootRightMargin - halfLeafWidth - halfStrokeWidth

          }

          (marker , Box(boxData, canopy))

        }
      }
    }

  //============================================================================================
  // MARKER HELPER CLASSES
  //

  protected class RenderMarker extends RenderData with Positioned {

    val arcRadius : Double = thisRenderer.arcRadius
    val halfLeafWidth : Double = thisRenderer.halfLeafWidth
    val halfStrokeWidth : Double = thisRenderer.halfStrokeWidth
    val internalPadding : Double = thisRenderer.internalPadding
    val externalPadding : Double = thisRenderer.externalPadding

    var rootX : Double = 0.0
    var rootY : Double = 0.0

    var internalWidth : Double = 0.0
    var internalHeight : Double = 0.0

    var rootLeftMargin : Double = 0.0
    var rootRightMargin : Double = 0.0

    var labelWidth : Double = 0.0
    var labelHeight : Double = 0.0

    var labelPadding : Double = 0.0

    var horizontalDependants : List[Positioned] = Nil
    var verticalDependants : List[Positioned] = Nil

    val edgeData : EdgeData = new EdgeData

    def edgeMarker : LayoutMarker = new LayoutMarker {

      val owner = edgeData
      val wasExternal = true

    }

  }

  class EdgeData extends Positioned {

    var startX : Double = 0.0
    var startY : Double = 0.0

    var endX : Double = 0.0
    var endY : Double = 0.0

    def x : Double = startX
    def x_=(d : Double) = startX = d

    def y : Double = startY
    def y_=(d : Double) = startY = d

    var horizontalDependants : List[Positioned] = Nil
    var verticalDependants : List[Positioned] = Nil

  }

  trait Positioned {

    var x : Double
    var y : Double

    var horizontalDependants : List[Positioned]
    var verticalDependants : List[Positioned]

    def shiftRight(amount : Double) : Unit = 
      if (amount != 0) {
        x += amount
        horizontalDependants foreach (_.shiftRight(amount))
      }

    def shiftDown(amount : Double) : Unit = 
      if (amount != 0) {
        y += amount
        verticalDependants foreach (_.shiftDown(amount))
      }

    def shiftLeft(amount : Double) : Unit = shiftRight(-amount)
    def shiftUp(amount : Double) : Unit = shiftDown(-amount)

  }

  abstract class LayoutMarker { thisMarker =>

    val owner : Positioned
    val wasExternal : Boolean

    val rootEdge : EdgeElement

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

    def leftEdge : Double = owner.rootX - leftMargin
    def rightEdge : Double = owner.rootX + rightMargin

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
