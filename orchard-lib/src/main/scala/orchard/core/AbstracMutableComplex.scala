/**
  * AbstractMutableComplex.scala - A Abstract Implementation of mutability
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

import scala.collection.mutable.HashMap

import Util._

abstract class AbstractMutableComplex[A](seed : NCell[A]) extends MutableComplex[A] {

  type CellType <: AbstractMutableCell

  //============================================================================================
  // COMPLEX IMPLEMENTATION
  //

  populateComplex(seed)

  //============================================================================================
  // CELL IMPLEMENTATION
  //

  abstract class AbstractMutableCell extends MutableCell { thisCell : CellType =>

    // Cell Data
    var canopy : Option[RoseTree[CellType, Int]] = None
    var target : Option[CellType] = None
    var sources : Option[Vector[CellType]] = None
    var container : Option[CellType] = None

    // Edge Data
    var incoming : Option[CellType] = None
    var outgoing : Option[CellType] = None

    // Panel tracking

    private val panelCellMap = new HashMap[Panel[A], Panel[A]#CellType]
    private val panelEdgeMap = new HashMap[Panel[A], Panel[A]#EdgeType]

    def cellPanels : Iterable[Panel[A]] = panelCellMap.keys
    def edgePanels : Iterable[Panel[A]] = panelEdgeMap.keys

    def cellOnPanel(panel : Panel[A]) : panel.CellType = panelCellMap(panel).asInstanceOf[panel.CellType]
    def edgeOnPanel(panel : Panel[A]) : panel.EdgeType = panelEdgeMap(panel).asInstanceOf[panel.EdgeType]

    def getOrCreateCell(panel : Panel[A]) : panel.CellType = 
      (panelCellMap get panel) match {
        case None => panel.newCell(thisCell.asInstanceOf[panel.complex.CellType])
        case Some(cell) => cell.asInstanceOf[panel.CellType]
      }

    def getOrCreateEdge(panel : Panel[A]) : panel.EdgeType = 
      (panelEdgeMap get panel) match {
        case None => panel.newEdge(thisCell.asInstanceOf[panel.complex.CellType])
        case Some(edge) => edge.asInstanceOf[panel.EdgeType]
      }

    def registerPanelCell(panel : Panel[A])(cell : panel.CellType) : Unit = { panelCellMap(panel) = cell }
    def registerPanelEdge(panel : Panel[A])(edge : panel.EdgeType) : Unit = { panelEdgeMap(panel) = edge }

    def unregisterPanelCell(panel : Panel[A]) : Unit = panelCellMap -= panel
    def unregisterPanelEdge(panel : Panel[A]) : Unit = panelEdgeMap -= panel

  }

}
