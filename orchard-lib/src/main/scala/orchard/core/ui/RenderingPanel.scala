/**
  * RenderingPanel.scala - A panel which renders its elements
  * 
  * @author Eric Finster
  * @version 0.1
  */

package orchard.core.ui

import scala.collection.mutable.ListBuffer

import orchard.core.util._
import orchard.core.complex._

import Util._

// Add a type class so that A is renderable??
trait RenderingPanel[A] extends Panel[A] {

  type CellType <: VisualCell
  type EdgeType <: VisualEdge

  //============================================================================================
  // VISUAL OPTIONS
  //

  var arcRadius : Double = 4

  var internalPadding : Double = 5
  var externalPadding : Double = 5

  var halfLeafWidth : Double = 4
  var halfStrokeWidth : Double = 1

  def strokeWidth = halfStrokeWidth + halfStrokeWidth
  def leafWidth = halfLeafWidth + strokeWidth + halfLeafWidth

  //============================================================================================
  // RENDERING SETUP AND CLEANUP
  //

  def refresh = {
    clearRenderState
    render
  }

  def clearRenderState =
  {
    baseCell foreachCell (cell => cell.clearRenderState)

    for { tgt <- baseCell.target } {
      tgt foreachEdge (edge => edge.clearRenderState)
    }
  }

  def setLabelSizes : Unit

  def render : Unit = {
    val baseMarker = baseCell.sources match {
        case None => render(baseCell, new Array(0))
        case Some(srcs) =>
          {
            val markers =
              (srcs map (src => new LayoutMarker { val owner = src ; val wasExternal = true }))

            render(baseCell, markers.toArray)
          }
      }

    // Now align the edges properly ...
    baseCell foreachCell (cell => cell.alignEdges)

    // ... and render them.
    for { tgt <- baseCell.target } {
      tgt foreachEdge (edge => edge.renderPath)
    }
  }
  

  def edgePadding = {
    if (baseCell.isExternal) {
      if (baseCell.sourceCount > 1) {
        val numRightEdges : Int = baseCell.sourceCount / 2
        // Ech.  This isn't very careful.  I think you can do better.
        val padding = ((numRightEdges - 1) * (leafWidth + (2 * strokeWidth))) + halfLeafWidth + (2 * strokeWidth)
        padding
      } else {
        0.0
      }
    } else {
      0.0
    }
  }

  def panelX = baseCell.x - edgePadding
  def panelY = baseCell.y - (2 * externalPadding)
  def panelHeight = baseCell.height + (4 * externalPadding)
  def panelWidth = baseCell.width + (2 * edgePadding)

  //============================================================================================
  // MAIN RENDERING ALGORITHM
  //

  protected def render(cell : CellType, sourceMarkers : Array[LayoutMarker]) : LayoutMarker = {

    val cellMarker : LayoutMarker = 
      cell.canopy match {
        case None => { // External case

          // An external cell needs only make room for its label
          cell.internalWidth = 0.0
          cell.internalHeight = 0.0

          cell.rootLeftMargin = 
            strokeWidth +
              internalPadding + 
              (cell.labelWidth / 2)

          cell.rootRightMargin = 
            (cell.labelWidth / 2) +
              internalPadding +
              strokeWidth

          if (sourceMarkers.length == 0) {
            // This is a drop, return an appropriate marker
            new LayoutMarker {
              val owner = cell
              val wasExternal = false
              override def height = cell.height
              override def leftInternalMargin = cell.rootLeftMargin - halfLeafWidth - halfStrokeWidth
              override def rightInternalMargin = cell.rootRightMargin - halfLeafWidth - halfStrokeWidth
            }
          } else if (sourceMarkers.length == 1) {
            val marker = sourceMarkers.head

            // Align the roots and set the horizontal dependency
            // N.b. - this shift is probably redundant, but whatevs
            marker.owner.alignTo(cell)
            cell.addHorizontalDependent(marker.owner)

            new LayoutMarker {
              val owner = cell
              val wasExternal = false

              override def height =
                marker.height + cell.height + (if (marker.wasExternal) 0 else externalPadding)

              override def leftSubtreeMargin =
                Math.max(0.0, marker.leftMargin - cell.rootLeftMargin)
              override def rightSubtreeMargin =
                Math.max(0.0, marker.rightMargin - cell.rootRightMargin)

              override def leftInternalMargin = cell.rootLeftMargin - halfLeafWidth - halfStrokeWidth
              override def rightInternalMargin = cell.rootRightMargin - halfLeafWidth - halfStrokeWidth
            }
          } else {

            def layoutLeft(markers : Array[LayoutMarker]) : Unit = {
              for (i <- Range(markers.length - 2, -1, -1)) {
                val lastMarker = markers(i + 1)
                val thisMarker = markers(i)

                thisMarker.owner
                  .horizontalShift(lastMarker.owner.rootX -
                                      lastMarker.leftMargin -
                                      externalPadding -
                                      thisMarker.rightMargin -
                                      thisMarker.owner.rootX)

                cell.addHorizontalDependent(thisMarker.owner)
              }
            }

            def layoutRight(markers : Array[LayoutMarker]) : Unit = {
              for (i <- Range(1, markers.length)) {
                val lastMarker = markers(i-1)
                val thisMarker = markers(i)

                thisMarker.owner
                  .horizontalShift(lastMarker.owner.rootX +
                                     lastMarker.rightMargin +
                                     externalPadding +
                                     thisMarker.leftMargin -
                                     thisMarker.owner.rootX)

                cell.addHorizontalDependent(thisMarker.owner)
              }
            }

            val isOdd = (sourceMarkers.length & 1) != 0

            val leftChildren = sourceMarkers.slice(0, sourceMarkers.length / 2)
            val rightChildren = 
                if (isOdd)
                  sourceMarkers.slice(sourceMarkers.length / 2 + 1, sourceMarkers.length)
                else
                  sourceMarkers.slice(sourceMarkers.length / 2, sourceMarkers.length)

            val firstMarker = sourceMarkers.head
            val lastMarker = sourceMarkers.last
            val leftChild = leftChildren.last
            val rightChild = rightChildren.head

            // Process the middle child
            val midChild = if (isOdd) sourceMarkers(sourceMarkers.length / 2) else null

            if (isOdd) {
              // Align the roots and set the horizontal dependency
              midChild.owner.alignTo(cell)
              cell.addHorizontalDependent(midChild.owner)
            }

            val midLeftOffset = 
              if (isOdd) midChild.leftMargin + externalPadding + leftChild.rightMargin else 0.0

            val midRightOffset =
              if (isOdd) midChild.rightMargin + externalPadding + rightChild.leftMargin else 0.0

            val leftChildShift : Double =
              Math.max(Math.max(midLeftOffset, leftChild.rightMargin + (externalPadding / 2)),
                       cell.rootLeftMargin + halfLeafWidth + halfStrokeWidth + arcRadius)

            val rightChildShift : Double =
              Math.max(Math.max(midRightOffset, rightChild.leftMargin + (externalPadding / 2)),
                       cell.rootRightMargin + halfLeafWidth + halfStrokeWidth + arcRadius)

            leftChild.owner
              .horizontalShift(cell.rootX - leftChildShift - leftChild.owner.rootX)

            rightChild.owner
              .horizontalShift(cell.rootX + rightChildShift - rightChild.owner.rootX)

            cell.addHorizontalDependent(leftChild.owner)
            cell.addHorizontalDependent(rightChild.owner)

            layoutLeft(leftChildren)
            layoutRight(rightChildren)

            new LayoutMarker {
              val owner = cell
              val wasExternal = false

              override def height =
                getMaxHeight(sourceMarkers) + cell.height +
              (if (allExternal(sourceMarkers)) 0.0 else externalPadding)

              override def leftSubtreeMargin =
                firstMarker.leftMargin - halfLeafWidth - halfStrokeWidth

              override def rightSubtreeMargin =
                lastMarker.rightMargin - halfLeafWidth - halfStrokeWidth

              override def leftInternalMargin =
                (cell.rootX - firstMarker.owner.rootX)

              override def rightInternalMargin =
                (lastMarker.owner.rootX - cell.rootX)
            }
          }
        }

        case Some(tree) => { // internal case

          // This inner loop traverse the source tree, recursively rendering the cells it finds
          def traverse[D <: Nat](canopyTree : RoseTree[CellType, Int]) : LayoutMarker =
            canopyTree match {
              case Rose(leafIndex) =>
                {
                  val thisMarker = sourceMarkers(leafIndex)

                  if (leafIndex == 0 && sourceMarkers.length == 1) {
                    thisMarker.truncateUnique
                  } else if (leafIndex == 0) {
                    thisMarker.truncateLeft
                  } else if (leafIndex == sourceMarkers.length - 1) {
                    thisMarker.truncateRight
                  } else {
                    thisMarker.truncateMiddle
                  }
                }

              case Branch(mcell, branches) =>
                {

                  // Render the branches first
                  val branchMarkers = branches map (branch => traverse(branch))

                  // I think this works ... align if it is an outermost glob or if
                  // the unique branch begins with a node
                  if (mcell.isLoop) {
                    if (! cell.isLoop || branches.head.rootElement.isDefined) {
                      val marker = branchMarkers.head
                      marker.owner.alignTo(mcell)
                      mcell.addHorizontalDependent(marker.owner)
                    }
                  }

                  // Now render the current cell
                  val thisMarker = render(mcell, branchMarkers.toArray)

                  // Finally, since mcell now reads the correct
                  // height, we can set the vertical dependencies
                  branchMarkers foreach
                    (marker => {
                       if (! marker.wasExternal) {
                         marker.owner
                           .verticalShift(mcell.rootY -
                                            mcell.height -
                                            externalPadding -
                                            marker.owner.rootY)

                         mcell.addVerticalDependent(marker.owner)
                       }
                     })

                  thisMarker
                }
            }

          // Traverse the source tree if necessary
          val rootMarker =
            if (cell.isObject) {
              val childObj = tree.rootElement.force("Malformed object!")
              render(childObj, new Array(0))
            } else {
              traverse(tree)
            }

          // Set the position of the root element of the tree
          if (! rootMarker.wasExternal) {
            rootMarker.owner.alignTo(cell)
            rootMarker.owner
              .verticalShift(cell.rootY -
                               internalPadding -
                               cell.labelHeight)

            cell.addHorizontalDependent(rootMarker.owner)
            cell.addVerticalDependent(rootMarker.owner)
          }

          // Now setup the dimensions of the current cell
          cell.internalWidth = rootMarker.width
          cell.internalHeight = rootMarker.height

          // This is how much *extra space* must be added for the label, but it should
          // have a minimum, which I guess is zero?
          cell.labelPadding = Math.max(0.0, cell.labelWidth - rootMarker.rightMargin) + internalPadding

          cell.rootLeftMargin = strokeWidth + internalPadding + rootMarker.leftMargin
          cell.rootRightMargin = rootMarker.rightMargin + cell.labelPadding + internalPadding + strokeWidth

          // Setup and return an appropriate marker
          if (sourceMarkers.length == 0) {
            // If there were no leaves, it's easy
            new LayoutMarker {
              val owner = cell
              val wasExternal = false

              override def height = cell.height
              override def leftInternalMargin = cell.rootLeftMargin - halfLeafWidth - halfStrokeWidth
              override def rightInternalMargin = cell.rootRightMargin - halfLeafWidth - halfStrokeWidth
            }
          } else {
            val firstMarker = sourceMarkers.head
            val lastMarker = sourceMarkers.last

            new LayoutMarker {
              val owner = cell
              val wasExternal = false

              override def height =
                getMaxHeight(sourceMarkers) + cell.height +
                  (if (allExternal(sourceMarkers)) 0.0 else externalPadding)

              override def leftSubtreeMargin = Math.max(0.0, myLeftEdge - leftOuterEdge)
              override def rightSubtreeMargin = Math.max(0.0, rightOuterEdge - myRightEdge)

              override def leftInternalMargin = cell.rootLeftMargin - halfLeafWidth - halfStrokeWidth
              override def rightInternalMargin = cell.rootRightMargin - halfLeafWidth - halfStrokeWidth

              // Auxillary calculations

              def leftOuterEdge = firstMarker.owner.rootX - firstMarker.leftMargin
              def rightOuterEdge = lastMarker.owner.rootX + lastMarker.rightMargin

              def myLeftEdge = cell.rootX - cell.rootLeftMargin
              def myRightEdge = cell.rootX + cell.rootRightMargin
            }
          }
        }
      }

    cellMarker
  }

  //============================================================================================
  // RENDERING UTILITY METHODS
  //

  def allExternal(markers : Array[LayoutMarker]) : Boolean =
    (true /: (markers map (_.wasExternal))) (_ && _)

  def getMaxHeight(markers : Array[LayoutMarker]) : Double =
    (0.0 /: (markers map (_.height))) (Math.max)

  def getTotalWidth(markers : Array[LayoutMarker]) : Double =
  {
    val padding : Double =
      if (markers.length > 1)
        externalPadding * markers.length - 1
      else
        0.0

    (padding /: (markers map (_.width))) (_ + _)
  }

  //============================================================================================
  // TRAITS AND CLASSES
  //

  trait Rooted {
    var rootX : Double
    var rootY : Double

    def horizontalShift(amount : Double)
    def verticalShift(amount : Double)

    def alignTo(other : Rooted) =
      horizontalShift(other.rootX - rootX)
  }

  //============================================================================================
  // VISUAL CELLS
  //

  trait VisualCell extends PanelCell with Rooted { thisCell : CellType =>

    var internalWidth : Double = 0.0
    var internalHeight : Double = 0.0

    var rootLeftMargin : Double = 0.0
    var rootRightMargin : Double = 0.0

    var rootX : Double = 0.0
    var rootY : Double = 0.0

    var labelWidth : Double = 0.0
    var labelHeight : Double = 0.0

    var labelPadding : Double = 0.0

    val vertDeps = new ListBuffer[Rooted]
    val horzDeps = new ListBuffer[Rooted]

    def addVerticalDependent(r : Rooted) = { vertDeps += r }
    def addHorizontalDependent(r : Rooted) = { horzDeps += r }

    def horizontalShift(amount : Double) =
    {
      if (amount != 0) {
        rootX += amount
        horzDeps foreach (dep => dep.horizontalShift(amount))
      }
    }

    def verticalShift(amount : Double) =
    {
      if (amount != 0) {
        rootY += amount
        vertDeps foreach (dep => dep.verticalShift(amount))
      }
    }

    // Derived visual data

    def labelOffset : Double = 
      rootLeftMargin + leafWidth + strokeWidth

    def width : Double =
      rootLeftMargin + rootRightMargin

    def height : Double =
      strokeWidth +
        internalPadding +
        internalHeight +
        labelHeight +
        internalPadding +
        strokeWidth

    def x : Double = rootX - rootLeftMargin
    def y : Double = rootY - height

    def centerX : Double = x + (width / 2)
    def centerY : Double = y + (height / 2)

    def clearRenderState =
    {
      internalWidth = 0.0
      internalHeight = 0.0
      rootLeftMargin = 0.0
      rootRightMargin = 0.0
      rootX = 0.0
      rootY = 0.0
      labelWidth = 0.0
      labelHeight = 0.0
      labelPadding = 0.0
      vertDeps.clear
      horzDeps.clear
    }

    def alignEdges = {
      if (isBase) {
        sources match {
          case None => ()
          case Some(srcs) => {
            // The x value should be set by the rendering pass ...
            srcs foreach (src => src.incomingY = y - (2 * externalPadding))
          }
        }

        target match {
          case None => ()
          case Some(tgt) => {
            tgt.outgoingX = rootX ; tgt.outgoingY = rootY + (2 * externalPadding)
          }
        }
      }

      if (isExternal) {
        sources match {
          case None => ()
          case Some(srcs) => {
            srcs foreach
            (src => {
               if (src.incomingX == rootX) {
                 src.outgoingX = rootX
                 src.outgoingY = y
               } else if (src.incomingX < rootX) {
                 src.outgoingX = x
                 src.outgoingY = centerY
               } else {
                 src.outgoingX = x + width
                 src.outgoingY = centerY
               }
             })
          }
        }

        target match {
          case None => ()
          case Some(tgt) => {
            tgt.incomingX = rootX
            tgt.incomingY = rootY
          }
        }
      }
    }

    //============================================================================================
    // SELECTION AND HIGHLIGHTING
    //

    def doHover : Unit = ()
    def doSelect : Unit = ()
    def doUnhover : Unit = ()
    def doDeselect : Unit = ()

    override def onEventEmitted(ev : CellEvent) =
      ev match {
        case RequestCellSelected => doSelect
        case RequestCellDeselected => doDeselect
        case RequestCellHovered => doHover
        case RequestCellUnhovered => doUnhover
        case _ => super.onEventEmitted(ev)
      }

    //============================================================================================
    // MISC
    //

    def boundInfo : String =
      "(x = " ++ x.toString ++
        ", y = " ++ y.toString ++
        ", width = " ++ width.toString ++
        ", height = " ++ height.toString ++ ")"

    def rootInfo : String =
      "(rootX = " ++ rootX.toString ++
        ", rootY = " ++ rootY.toString ++
        ", rootLeftMargin = " ++ rootLeftMargin.toString ++
        ", rootRightMargin = " ++ rootRightMargin.toString ++
        ")"
  }

  //============================================================================================
  // VISUAL EDGES
  //

  trait VisualEdge extends PanelEdge with Rooted { thisEdge : EdgeType =>

    var incomingX : Double = 0.0
    var incomingY : Double = 0.0
    var outgoingX : Double = 0.0
    var outgoingY : Double = 0.0

    def rootX = incomingX
    def rootX_=(r : Double) = { incomingX = r }

    def rootY = incomingY
    def rootY_=(r : Double) = { incomingY = r }

    def horizontalShift(amount : Double) = { rootX += amount }
    def verticalShift(amount : Double) = { rootY += amount }

    def renderPath : Unit

    def isVertical = incomingX == outgoingX

    def clearRenderState =
    {
      incomingX = 0.0
      incomingY = 0.0
      outgoingX = 0.0
      outgoingY = 0.0
    }

    def doHover : Unit = ()
    def doSelect : Unit = ()
    def doUnhover : Unit = ()
    def doDeselect : Unit = ()

    override def onEventEmitted(ev : CellEvent) = {
      // Edges consume their events.  I think this might
      // have been fucking up the event stream.  But seriously,
      // you need to fix it.
      ev match {
        case RequestEdgeSelected => doSelect
        case RequestEdgeDeselected => doDeselect
        case RequestEdgeHovered => doHover
        case RequestEdgeUnhovered => doUnhover
        case _ => ()
      }
    }

    def renderInfo : String =
      "(incomingX = " ++ incomingX.toString ++
        ", incomingY = " ++ incomingY.toString ++
        ", outgoingX = " ++ outgoingX.toString ++
        ", outgoingY = " ++ outgoingY.toString ++
        ")"
  }

  //============================================================================================
  // LAYOUT MARKERS
  //

  abstract class LayoutMarker { thisMarker =>
    val owner : Rooted

    val depth : Int = 0
    val wasExternal : Boolean

    def height : Double = 0

    def leftSubtreeMargin : Double = 0
    def rightSubtreeMargin : Double = 0
    def leftInternalMargin : Double = 0
    def rightInternalMargin : Double = 0

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
