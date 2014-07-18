/**
  * CellComplex.scala - A Cell Complex
  * 
  * @author Eric Finster
  * @version 0.1
  */

package orchard.core.complex

import scala.collection.mutable.ListBuffer

import orchard.core.cell._
import orchard.core.util._

import Util._

trait CellComplex[A] { thisComplex =>

  type CellType <: ComplexCell

  def newCell(item : A) : CellType

  // This method is not necessary.  What we should do instead
  // is keep a reference to the ncell itself, from which all of
  // this information can be reconstructed ...
  def baseCells : Vector[CellType]

  def topCell : CellType = baseCells.last
  def dimension : Int = baseCells.length - 1
  def apply(idx : Int) : CellType = baseCells(idx)
  def toCell : NCell[A] = topCell.skeleton map (_.item)

  def forAllCells(action : CellType => Unit) : Unit = {
    baseCells foreach (base => base foreachCell action)
  }

  def forAllCells(startDim : Int, endDim : Int, action : CellType => Unit) = {
    baseCells.slice(startDim, endDim) foreach (base => base foreachCell action)
  }

  def foreachProperFace(action : CellType => Unit) : Unit = {
    forAllCells(0, dimension, action)
  }

  def forAllFaces(action : CellType => Unit) : Unit = 
    if (dimension > 0)
      baseCells(dimension - 1) foreachCell action

  def seek(addr : CellAddress) : Option[CellType] = 
    addr match {
      case Immediate => Some(topCell)
      case Target(prefix) => 
        for { 
          pref <- seek(prefix) 
          tgt <- pref.target 
        } yield tgt
      // case Source(Immediate, loc) => 
      //   Some((topCell.sources.get)(loc.head))
      case Source(prefix, loc) =>
        for { 
          pref <- seek(prefix) 
          ptr <- new RoseZipper(pref.totalCanopy, Nil).seek(loc)
        } yield {
          ptr.focus match {
            case Rose(idx) => (pref.sources.get)(idx) 
            case Branch(cell, _) => cell.target.get
          }
        }
    }

  //============================================================================================
  // COMPLEX CELLS
  //

  trait ComplexCell 
      extends CellBase[CellType, CellType] 
      with    EdgeBase[CellType, CellType] { thisCell : CellType =>
      

    def item : A
    def complex : CellComplex[A] = thisComplex

    // That these are mutable here is dubious ...
    var loops : List[CellType] = Nil
    var skeleton : NCell[CellType] = null

    //============================================================================================
    // SEMANTIC ROUTINES
    //

    // We should be able to remove this ListBuffer dependency here.  The point should be the 
    // traversal routines that you implemented and flatmap and what have you.  But what ever
    // for right now ...
    def neighborhood : List[CellType] = {
      val neighbors = new ListBuffer[CellType]
      topCell.skeleton map (face => { if (face.hasFace(this)) { neighbors += face } })
      neighbors.toList
    }

    def hasFace(cell : CellType) : Boolean = {
      var found : Boolean = false
      skeleton map (face => { found = found | (face == cell) })
      found
    }

    def properTargets : List[CellType] =
      target match {
        case None => Nil
        case Some(tgt) => tgt :: tgt.properTargets
      }

    def dimension : Int = properTargets.length

    def targets : List[CellType] =
      (this :: properTargets).reverse

    def isTopCell : Boolean = (isExternal && isBase && loops == Nil)
    def isArrow : Boolean =
      target match {
        case None => false
        case Some(cell) => cell.isObject
      }

    def topCell : CellType = {
      if (this.isTopCell) this else {
        val next =
          if (loops != Nil) {
            loops.head
          } else {
            baseContainer.incoming.get
          }

        next.topCell
      }
    }

    def toNCell : NCell[A] = skeleton map (_.item)

    def extractCanopy(verticalBoundary : Vector[CellType]) : RoseTree[CellType, Int] = {

      def verticalTrace(cell : CellType, lvs : Vector[RoseTree[CellType, Int]]) : RoseTree[CellType, Int] = {
        if ((verticalBoundary contains cell) || cell.isExternal) {
          Branch(cell, lvs)
        } else {

          def horizontalTrace(tr : RoseTree[CellType, Int]) : RoseTree[CellType, Int] =
            tr match {
              case Rose(idx) => if (cell.isObject) { Rose(idx) } else lvs(idx)
              case Branch(hCell, hBranches) => {
                verticalTrace(hCell, hBranches map horizontalTrace)
              }
            }

          horizontalTrace(cell.canopy.get)
        }
      }

      val startLeaves = Range(0, sourceCount) map (i => Rose(i))
      verticalTrace(thisCell, startLeaves.toVector)

    }

    def totalCanopy : RoseTree[CellType, Int] =
      extractCanopy(Vector.empty)

    def sourceTree : Option[RoseTree[CellType, Int]] = 
      for {
        tgt <- target
        srcs <- sources
      } yield {
        tgt.extractCanopy(srcs)
      }

    def address : CellAddress = {
      if (thisCell.dimension == thisComplex.dimension) {
        Immediate
      } else if (thisCell.dimension == thisComplex.dimension - 1) {
        if (! isExternal) {
          Target(Immediate)
        } else {
          Source(Immediate, List(topCell.sources.get indexOf thisCell))
        }
      } else {

        def buildPrefix(i : Int) : AddressPrefix =
          if (i <= 0) Immediate else Target(buildPrefix(i - 1))

        if (isBase) {
          buildPrefix(thisComplex.dimension - thisCell.dimension)
        } else {
          // This should be the generic case.  We look at the base cell two dimensions higher,
          // and get it's source tree.  Then we look for this guy as an edge in that rose tree

          val refCell = baseCells(thisCell.dimension + 2)
          val refSrcs = baseCells(thisCell.dimension + 1).sources.get

          val ptr = new RoseZipper(refCell.sourceTree.get, Nil).find(
            branchCell => {
              branchCell.target == Some(thisCell)
            },
            roseIdx => {
              refSrcs(roseIdx) == thisCell
            }).get


          val prefix : AddressPrefix =
            buildPrefix(thisComplex.dimension - thisCell.dimension - 1)

          Source(prefix, ptr.toAddr)
        }
      }
    }
  }
}


