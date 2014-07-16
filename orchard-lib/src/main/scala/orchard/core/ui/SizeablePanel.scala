/**
  * SizeablePanel.scala - A Panel which renders itself to expressions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.ui

import scala.collection.mutable.ListBuffer

import orchard.core.util._

trait SizeablePanel[A] extends Panel[A] {

  type SizeResult = SizeExpression[Int]

  type CellType <: SizeableCell
  type EdgeType <: SizeableEdge

  //============================================================================================
  // VISUAL OPTIONS
  //

  var arcRadius : SizeResult = Constant(4)

  var internalPadding : SizeResult = Constant(5)
  var externalPadding : SizeResult = Constant(5)

  var halfLeafWidth : SizeResult = Constant(4)
  var halfStrokeWidth : SizeResult = Constant(1)

  def strokeWidth = halfStrokeWidth + halfStrokeWidth
  def leafWidth = halfLeafWidth + strokeWidth + halfLeafWidth

  //============================================================================================
  // RENDERING SETUP AND CLEANUP
  //

  def refresh = {
    clearRenderState
    render
  }

  def clearRenderState = {
    baseCell foreachCell (cell => cell.clearRenderState)

    for { tgt <- baseCell.target } {
      tgt foreachEdge (edge => edge.clearRenderState)
    }
  }

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

    // In the sizeable version, I really don't think that we need this, do we???

    // ... and render them.
    for { tgt <- baseCell.target } {
      tgt foreachEdge (edge => edge.renderPath)
    }
  }

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
                Max(0.0, marker.leftMargin - cell.rootLeftMargin)
              override def rightSubtreeMargin =
                Max(0.0, marker.rightMargin - cell.rootRightMargin)

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

            val midLeftOffset : SizeResult = 
              if (isOdd) midChild.leftMargin + externalPadding + leftChild.rightMargin else 0.0

            val midRightOffset : SizeResult =
              if (isOdd) midChild.rightMargin + externalPadding + rightChild.leftMargin else 0.0

            val leftChildShift : SizeResult =
              Max(Max(midLeftOffset, leftChild.rightMargin + (externalPadding / 2)),
                cell.rootLeftMargin + halfLeafWidth + halfStrokeWidth + arcRadius)

            val rightChildShift : SizeResult =
              Max(Max(midRightOffset, rightChild.leftMargin + (externalPadding / 2)),
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
              val childObj = tree.rootElement.get
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
          cell.labelPadding = Max[Int](0.0, cell.labelWidth - rootMarker.rightMargin) + internalPadding

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

              override def leftSubtreeMargin = Max(0.0, myLeftEdge - leftOuterEdge)
              override def rightSubtreeMargin = Max(0.0, rightOuterEdge - myRightEdge)

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
  // VISUAL PANEL INFORMATION
  //

  def edgePadding : SizeResult = {
    if (baseCell.isExternal) {
      if (baseCell.sourceCount > 1) {
        val numRightEdges : Int = baseCell.sourceCount / 2

        // Ech.  This isn't very careful.  I think you can do better.
        ((leafWidth + (strokeWidth * 2)) * (numRightEdges - 1)) + halfLeafWidth + (strokeWidth * 2)
      } else {
        0.0
      }
    } else {
      0.0
    }
  }

  def panelX = baseCell.x - edgePadding
  def panelY = baseCell.y - (externalPadding * 2)
  def panelHeight = baseCell.height + (externalPadding * 4)
  def panelWidth = baseCell.width + (edgePadding * 2)

  //============================================================================================
  // RENDERING UTILITY METHODS
  //

  def allExternal(markers : Array[LayoutMarker]) : Boolean =
    (true /: (markers map (_.wasExternal))) (_ && _)

  def getMaxHeight(markers : Array[LayoutMarker]) : SizeResult =
    ((0.0 : SizeResult) /: (markers map (_.height))) (Max(_, _)) 

  def getTotalWidth(markers : Array[LayoutMarker]) : SizeResult =
  {
    val padding : SizeResult =
      if (markers.length > 1)
        externalPadding * (markers.length - 1)
      else
        0.0

    (padding /: (markers map (_.width))) (_ + _)
  }

  //============================================================================================
  // TRAITS AND CLASSES
  //

  trait Rooted {

    var rootX : SizeResult
    var rootY : SizeResult

    def horizontalShift(amount : SizeResult)
    def verticalShift(amount : SizeResult)

    def alignTo(other : Rooted) =
      horizontalShift(other.rootX - rootX)

  }

  //============================================================================================
  // SIZEABLE CELLS
  //

  trait SizeableCell extends PanelCell with Rooted { thisCell : CellType =>

    var myInternalWidth : SizeResult = 0.0
    var myInternalHeight : SizeResult = 0.0

    def internalWidth : SizeResult = Attribute(thisCell.hashCode, "myInternalWidth")
    def internalWidth_=(size : SizeResult) : Unit = 
      myInternalWidth = size

    def internalHeight : SizeResult = Attribute(thisCell.hashCode, "myInternalHeight")
    def internalHeight_=(size : SizeResult) : Unit = 
      myInternalHeight = size

    var myRootLeftMargin : SizeResult = 0.0
    var myRootRightMargin : SizeResult = 0.0

    def rootLeftMargin : SizeResult = Attribute(thisCell.hashCode, "rootLeftMargin")
    def rootLeftMargin_=(size : SizeResult) : Unit =
      myRootLeftMargin = size

    def rootRightMargin : SizeResult = Attribute(thisCell.hashCode, "rootRightMargin")
    def rootRightMargin_=(size : SizeResult) : Unit =
      myRootLeftMargin = size

    var myRootX : SizeResult = 0.0
    var myRootY : SizeResult = 0.0

    override def rootX : SizeResult = Attribute(thisCell.hashCode, "rootX")
    override def rootX_=(size : SizeResult) : Unit =
      myRootX = size

    override def rootY : SizeResult = Attribute(thisCell.hashCode, "rootY")
    override def rootY_=(size : SizeResult) : Unit =
      myRootY = size

    def labelWidth : SizeResult = Attribute(thisCell.hashCode, "labelWidth")
    def labelHeight : SizeResult = Attribute(thisCell.hashCode, "labelHeight")

    var myLabelPadding : SizeResult = 0.0

    def labelPadding : SizeResult = Attribute(thisCell.hashCode, "labelPadding")
    def labelPadding_=(size : SizeResult) : Unit =
      myLabelPadding = size

    val vertDeps = new ListBuffer[Rooted]
    val horzDeps = new ListBuffer[Rooted]

    def addVerticalDependent(r : Rooted) = { vertDeps += r }
    def addHorizontalDependent(r : Rooted) = { horzDeps += r }

    def horizontalShift(amount : SizeResult) = {
      rootX += amount
      horzDeps foreach (_.horizontalShift(amount))
    }

    def verticalShift(amount : SizeResult) = {
      rootY += amount
      vertDeps foreach (_.verticalShift(amount))
    }

    // Derived visual data

    def labelOffset : SizeResult = 
      rootLeftMargin + leafWidth + strokeWidth

    def width : SizeResult =
      rootLeftMargin + rootRightMargin

    def height : SizeResult =
      strokeWidth +
        internalPadding +
        internalHeight +
        labelHeight +
        internalPadding +
        strokeWidth

    def x : SizeResult = rootX - rootLeftMargin
    def y : SizeResult = rootY - height

    def centerX : SizeResult = x + (width / 2)
    def centerY : SizeResult = y + (height / 2)

    def clearRenderState =
    {
      internalWidth = 0.0
      internalHeight = 0.0
      rootLeftMargin = 0.0
      rootRightMargin = 0.0
      rootX = 0.0
      rootY = 0.0
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
            srcs foreach (src => src.incomingY = y - (externalPadding * 2))
          }
        }

        target match {
          case None => ()
          case Some(tgt) => {
            tgt.outgoingX = rootX ; tgt.outgoingY = rootY + (externalPadding * 2)
          }
        }
      }

      if (isExternal) {
        sources match {
          case None => ()
          case Some(srcs) => {
            srcs foreach
            (src => {
              src.outgoingX = 
                If(
                  src.incomingX === rootX, 
                  rootX, 
                  If( 
                    src.incomingX < rootX,
                    x,
                    x + width
                  ))
                    
              src.outgoingY =
                If(
                  src.incomingX === rootX,
                  y,
                  If(
                    src.incomingX < rootX,
                    centerY,
                    centerY
                  ))
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

  }

  //============================================================================================
  // VISUAL EDGES
  //

  trait SizeableEdge extends PanelEdge with Rooted { thisEdge : EdgeType =>

    var incomingX : SizeResult = 0.0
    var incomingY : SizeResult = 0.0
    var outgoingX : SizeResult = 0.0
    var outgoingY : SizeResult = 0.0

    def rootX = incomingX
    def rootX_=(r : SizeResult) = { incomingX = r }

    def rootY = incomingY
    def rootY_=(r : SizeResult) = { incomingY = r }

    def horizontalShift(amount : SizeResult) = { rootX += amount }
    def verticalShift(amount : SizeResult) = { rootY += amount }

    def renderPath : Unit

    def isVertical = incomingX === outgoingX

    def clearRenderState =
    {
      incomingX = 0.0
      incomingY = 0.0
      outgoingX = 0.0
      outgoingY = 0.0
    }

  }


  //============================================================================================
  // LAYOUT MARKERS
  //

  abstract class LayoutMarker { thisMarker =>
    val owner : Rooted

    val depth : Int = 0
    val wasExternal : Boolean

    def height : SizeResult = 0

    def leftSubtreeMargin : SizeResult = 0
    def rightSubtreeMargin : SizeResult = 0
    def leftInternalMargin : SizeResult = 0
    def rightInternalMargin : SizeResult = 0

    def leftMargin : SizeResult =
      leftSubtreeMargin + leftInternalMargin + halfLeafWidth + halfStrokeWidth

    def rightMargin : SizeResult =
      rightSubtreeMargin + rightInternalMargin + halfLeafWidth + halfStrokeWidth

    def width : SizeResult =
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

    // override def toString = "LM(" ++ owner.toString ++ ")" ++ 
    //   "(we = " ++ wasExternal.toString ++ ", ht = " ++ height.toString ++
    //   ", lsm = " ++ leftSubtreeMargin.toString ++
    //   ", lim = " ++ leftInternalMargin.toString ++
    //   ", rim = " ++ rightInternalMargin.toString ++
    //   ", rsm = " ++ rightSubtreeMargin.toString ++ ")"
  }
  
}
