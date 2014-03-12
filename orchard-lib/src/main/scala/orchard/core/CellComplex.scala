/**
  * CellComplex.scala - A Cell Complex
  * 
  * @author Eric Finster
  * @version 0.1
  */

package orchard.core

import scala.collection.mutable.ListBuffer

import Util._

trait CellComplex[A] extends EventEmitter[CellEvent] { thisComplex =>

  type CellType <: ComplexCell

  def newCell(item : A) : CellType

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

  //============================================================================================
  // COMPLEX CELLS
  //

  trait ComplexCell 
      extends CellBase[CellType, CellType] 
      with    EdgeBase[CellType, CellType] 
      with    EventEmitter[CellEvent] { thisCell : CellType =>

    def item : A
    def complex : CellComplex[A] = thisComplex

    // That these are mutable here is dubious ...
    var loops : List[CellType] = Nil
    var skeleton : NCell[CellType] = null

    //============================================================================================
    // PANEL TRACKING
    //

    // The idea here is that instead of events, a cell complex tracks the panels which it has
    // be incarnated on.  This provides a more direct link between the complex and it's views,
    // and I think will have some benefits when we work on the mutability routines ....

    // Note that the reason it is not a map is that we have a dependent return type ...

    def cellPanels : Iterable[Panel[A]]
    def edgePanels : Iterable[Panel[A]]

    def cellOnPanel(panel : Panel[A]) : panel.CellType
    def edgeOnPanel(panel : Panel[A]) : panel.EdgeType

    def getOrCreateCell(panel : Panel[A]) : panel.CellType
    def getOrCreateEdge(panel : Panel[A]) : panel.EdgeType

    def registerPanelCell(panel : Panel[A])(cell : panel.CellType) : Unit
    def registerPanelEdge(panel : Panel[A])(edge : panel.EdgeType) : Unit

    def unregisterPanelCell(panel : Panel[A]) : Unit
    def unregisterPanelEdge(panel : Panel[A]) : Unit
    
    //============================================================================================
    // EVENT EMISSION
    //

    def emitToFaces(ev : CellEvent) : Unit =
      skeleton map (face => { face.emit(ev) })

    def emitToNeighborhood(ev : CellEvent) : Unit =
      topCell.skeleton map (face => { if (face.hasFace(this)) face.emit(ev) })

    //============================================================================================
    // SEMANTIC ROUTINES
    //

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
            baseContainer.incoming.force
          }

        next.topCell
      }
    }

    //============================================================================================
    // XML CONVERSION
    //

    def cellToXML(implicit vs : XmlSerializable[A]) = 
      skeleton.cell match {
        case Object(_, ev) => <obj id={hashCode.toString}><label>{vs.toXML(item)}</label></obj>
        case Composite(_, srcTree, tgtValue, ev) => {
          <cell id={hashCode.toString}><sourcetree>{

            def processSourceTree(tree : CellTree[_ <: Nat, CellType]) : xml.NodeSeq = 
              tree match {
                case Seed(o, _) => <seed ref={o.value.hashCode.toString} />
                case Leaf(l, _) => <leaf ref={l.value.hashCode.toString} />
                case Graft(c, brs, _) => <graft ref={c.value.hashCode.toString}>{ brs map processSourceTree }</graft>
              }

            processSourceTree(srcTree)
          }</sourcetree><target ref={tgtValue.hashCode.toString} /><label>{vs.toXML(item)}</label></cell>
        }
      }
  }

}


