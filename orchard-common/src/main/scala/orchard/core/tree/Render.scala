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

abstract class Renderer[T, A](implicit isNumeric : Numeric[T]) {

  import isNumeric._

  //============================================================================================
  // RENDERING OPTIONS
  //

  def halfLeafWidth : T
  def halfStrokeWidth : T

  def strokeWidth = fromInt(2) * halfStrokeWidth
  def leafWidth = fromInt(2) * halfLeafWidth

  def internalPadding : T
  def externalPadding : T

  //============================================================================================
  // ELEMENT TYPES
  //

  type ExternalBoxType <: ExternalBox
  type InternalBoxType <: InternalBox

  //============================================================================================
  // ELEMENT CONSTRUCTORS
  //

  def createExternalBox(lbl : A) : ExternalBox
  def createInternalBox(lbl : A, layout : BoxLayout) : InternalBox

  //============================================================================================
  // ELEMENT CLASSES
  //

  trait Rooted {

    var rootX : T = zero
    var rootY : T = zero

    var horizontalDependants : List[Rooted] = Nil
    var verticalDependants : List[Rooted] = Nil

    def shiftRight(amount : T) : Unit = {
      if (amount != 0) {
        rootX = (rootX + amount)
        horizontalDependants foreach (_.shiftRight(amount))
      }
    }

    def shiftDown(amount : T) : Unit = 
      if (amount != 0) {
        rootY = (rootY + amount)
        verticalDependants foreach (_.shiftDown(amount))
      }

    def shiftLeft(amount : T) : Unit = shiftRight(-amount)
    def shiftUp(amount : T) : Unit = shiftDown(-amount)

  }

  trait Edge {

    var startX : T
    var startY : T

    var endX : T
    var endY : T

  }

  trait BoxLayout {

    def width : T = leftMargin + rightMargin
    def height : T

    def leftMargin : T
    def rightMargin : T

  }

  trait LabeledBox extends BoxLayout with Rooted {

    def halfLabelWidth : T
    def halfLabelHeight : T

    def labelWidth : T = halfLabelWidth * fromInt(2)
    def labelHeight : T = halfLabelHeight * fromInt(2)

    def labelContainerWidth : T = fromInt(2) * internalPadding + labelWidth
    def labelContainerHeight : T = fromInt(2) * internalPadding + labelHeight

  }

  trait InternalBox extends LabeledBox {

    def interior : BoxLayout

    override def height : T = 
      strokeWidth + 
        labelContainerHeight + 
        interior.height +
        internalPadding +
        strokeWidth

    override def leftMargin : T = 
      interior.leftMargin + 
        internalPadding +
        strokeWidth

    override def rightMargin : T = 
      max(
        labelContainerWidth + strokeWidth,
        interior.rightMargin + internalPadding + strokeWidth
      )

  }

  trait ExternalBox extends LabeledBox {

    override def height : T = 
      strokeWidth + 
        internalPadding +
        labelHeight + 
        internalPadding +
        strokeWidth

    override def leftMargin : T = halfLabelWidth + internalPadding + strokeWidth
    override def rightMargin : T = halfLabelWidth + internalPadding + strokeWidth

  }

  //============================================================================================
  // OBJECT NESTING RENDERING
  //

  def renderObjectNesting(nst : Nesting[_0, A]) : Nesting[_0, LabeledBox] = 
    nst match {
      case Obj(a) => Obj(createExternalBox(a))
      case Box(a, Pt(c)) => {
        val interior = renderObjectNesting(c)
        val box = createInternalBox(a, interior.label)
        Box(box, Pt(interior))
      }
    }

  //============================================================================================
  // POSITIVE DIMENSIONAL NESTING RENDERING
  //

  def renderNesting[N <: Nat](nst : Nesting[S[N], A], lvs : Tree[N, LayoutMarker])
      : Option[(LayoutMarker, Nesting[S[N], LabeledBox])] =
    nst match {
      case Dot(a, c) => {

        val dot = createExternalBox(a)

        val leafMarkers = lvs.nodes
        val leafCount = leafMarkers.length

        val marker =
          if (leafCount == 0) {  // This is a drop. Simply return an appropriate marker ...

            new LayoutMarker {

              val element = dot
              val external = false

              override def height = dot.height
              override def leftInternalMargin = dot.leftMargin 
              override def rightInternalMargin = dot.rightMargin 

            }

          } else { // We have children.  Arrange them and calculate the marker.

            val isOdd = (leafCount & 1) != 0

            val firstMarker = leafMarkers.head
            val lastMarker = leafMarkers.last

            val midChild = leafMarkers(leafCount / 2)
            if (isOdd) dot.horizontalDependants :+= midChild.element

            if (leafCount > 1) {

              val leftChildren = leafMarkers.slice(0, leafCount / 2)
              val rightChildren = leafMarkers.slice(leafCount / 2 + (leafCount & 1), leafCount)

              val leftChild = leftChildren.last
              val rightChild = rightChildren.head

              val midLeftOffset = if (isOdd) midChild.leftMargin + externalPadding + leftChild.rightMargin else zero
              val midRightOffset = if (isOdd) midChild.rightMargin + externalPadding + rightChild.leftMargin else zero

              val leftChildShift = max(max(midLeftOffset, leftChild.rightMargin + externalPadding), dot.leftMargin)
              val rightChildShift = max(max(midRightOffset, rightChild.leftMargin + externalPadding), dot.rightMargin)

              val leftEdge =
                (leftChildren foldRight leftChildShift)({
                  case (currentMarker, leftShift) => {
                    val thisShift = leftShift + externalPadding + currentMarker.rightMargin
                    currentMarker.element.shiftLeft(thisShift)
                    dot.horizontalDependants :+= currentMarker.element
                    thisShift + currentMarker.leftMargin
                  }
                })

              val rightEdge =
                (rightChildren foldLeft rightChildShift)({
                  case (rightShift, currentMarker) => {
                    val thisShift = rightShift + externalPadding + currentMarker.leftMargin
                    currentMarker.element.shiftRight(thisShift)
                    dot.horizontalDependants :+= currentMarker.element
                    thisShift + currentMarker.rightMargin
                  }
                })

            }

            val (maxLeafHeight, allExternal) : (T, Boolean) =
              (leafMarkers map (m => (m.height, m.external)) foldLeft (zero, true))({
                case ((x , b),(y, c)) => (max(x, y), b && c)
              })

            new LayoutMarker {

              val element = dot
              val external = false

              override def height = maxLeafHeight + dot.height + (if (allExternal) zero else externalPadding)
              override def leftSubtreeMargin = firstMarker.leftMargin 
              override def rightSubtreeMargin = lastMarker.rightMargin
              override def leftInternalMargin = - firstMarker.element.rootX
              override def rightInternalMargin = lastMarker.element.rootX

            }

          }

        Some(marker, Dot(dot, c))

      }

      case Box(a, c) => {

        val (leafCount, leavesWithIndices) = lvs.zipWithIndex

        def verticalPass(tr : Tree[S[N], Nesting[S[N], A]]) : Option[(LayoutMarker, Tree[S[N], Nesting[S[N], LabeledBox]])] =
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

  //               // Indeed.  Here what we are going to do is fold over the children.  We do three things:
  //               //
  //               //  1) Set their vertical positions
  //               //  2) Find the leftmost and rightmost marker so that we can correctly setup the return maker
  //               //  3) Find the highest branch
  //               //

                val descendantMarkers : List[LayoutMarker] = layoutTree.nodes

                val (leftMostChild, rightMostChild, heightOfChildren) =
                  (descendantMarkers foldLeft (localLayout, localLayout, zero))({
                    case ((lcMarker, rcMarker, ht), thisMarker) => {

                      if (! thisMarker.external) {
                        thisMarker.element.shiftUp(localLayout.height)
                        localLayout.element.verticalDependants :+= thisMarker.element
                      }

                      val newLeftChild = if (thisMarker.leftEdge < lcMarker.leftEdge) thisMarker else lcMarker
                      val newRightChild = if (thisMarker.rightEdge < lcMarker.rightEdge) thisMarker else rcMarker

                      (newLeftChild, newRightChild, max(ht, thisMarker.height))

                    }
                  })

                val marker = new LayoutMarker {

                  val element = localLayout.element
                  val external = false

                  override def height = localLayout.height + heightOfChildren

                  override def leftInternalMargin = - (leftMostChild.element.rootX - leftMostChild.leftInternalMargin)
                  override def rightInternalMargin = rightMostChild.element.rootX + rightMostChild.rightInternalMargin

                  override def leftSubtreeMargin = leftMostChild.leftSubtreeMargin
                  override def rightSubtreeMargin = rightMostChild.rightSubtreeMargin

                }

                (marker, Node(resultNesting, resultTree))

              }
          }

        for {
          internalResult <- verticalPass(c)
        } yield {

          val (layout, canopy) = internalResult

          val box = createInternalBox(a, layout)
          box.horizontalDependants :+= layout.element  // Probably this will change ...

          // Setup and return an appropriate marker
          val marker = new LayoutMarker {

            val element = box
            val external = false

            override def height = box.height
            override def leftInternalMargin = box.leftMargin 
            override def rightInternalMargin = box.rightMargin

          }

          (marker , Box(box, canopy))

        }
      }
    }


  //============================================================================================
  // LAYOUT CLASS
  //

  abstract class LayoutMarker extends BoxLayout { thisMarker =>

    val element : Rooted
    val external : Boolean

    def height : T = zero

    def leftSubtreeMargin : T = zero
    def rightSubtreeMargin : T = zero
    def leftInternalMargin : T = zero
    def rightInternalMargin : T = zero

    def leftMargin : T = leftSubtreeMargin + leftInternalMargin 
    def rightMargin : T = rightSubtreeMargin + rightInternalMargin

    def leftEdge : T = element.rootX - leftMargin
    def rightEdge : T = element.rootY + rightMargin

    // Truncations

    def truncateLeft : LayoutMarker =
      new LayoutMarker {
        val element = thisMarker.element
        val external = true
        override def rightSubtreeMargin = thisMarker.rightSubtreeMargin
        override def rightInternalMargin = thisMarker.rightInternalMargin
      }

    def truncateRight : LayoutMarker =
      new LayoutMarker {
        val element = thisMarker.element
        val external = true
        override def leftSubtreeMargin = thisMarker.leftSubtreeMargin
        override def leftInternalMargin = thisMarker.leftInternalMargin
      }

    def truncateUnique : LayoutMarker =
      new LayoutMarker {
        val element = thisMarker.element
        val external = true
      }

    def truncateMiddle : LayoutMarker =
      new LayoutMarker {
        val element = thisMarker.element
        val external = true
        override def leftSubtreeMargin = thisMarker.leftSubtreeMargin
        override def rightSubtreeMargin = thisMarker.rightSubtreeMargin
        override def leftInternalMargin = thisMarker.leftInternalMargin
        override def rightInternalMargin = thisMarker.rightInternalMargin
      }

    override def toString = "LM(" ++ element.toString ++ ")" ++ 
      "(we = " ++ external.toString ++ ", ht = " ++ height.toString ++
      ", lsm = " ++ leftSubtreeMargin.toString ++
      ", lim = " ++ leftInternalMargin.toString ++
      ", rim = " ++ rightInternalMargin.toString ++
      ", rsm = " ++ rightSubtreeMargin.toString ++ ")"

  }

}
