/**
  * Panel.scala - A collection of cells and edges
  * 
  * @author Eric Finster
  * @version 0.1
  */

package orchard.core

import Util._

trait Panel[A] extends EventConduit[CellEvent] {

  type CellType <: PanelCell
  type EdgeType <: PanelEdge

  type ComplexType <: CellComplex[A]

  val complex : ComplexType

  def baseCell : CellType

  def newCell(owner : complex.CellType) : CellType
  def newEdge(owner : complex.CellType) : EdgeType

  //============================================================================================
  // CELL AND EDGE TRAITS
  //

  trait PanelCell extends CellBase[CellType, EdgeType] with EventConduit[CellEvent] { thisCell : CellType =>

    def owner : complex.CellType
    def item : A = owner.item

    reactTo(owner)

  }

  trait PanelEdge extends EdgeBase[CellType, EdgeType] with EventConduit[CellEvent] { thisEdge : EdgeType =>

    def owner : complex.CellType
    def item : A = owner.item

    reactTo(owner)

  }

  //============================================================================================
  // PANEL GENERATION
  //

  def generatePanelData(cell : complex.CellType, sources : Option[List[EdgeType]]) : CellType = {
    cell.shell match {
      case None => 
        {
          val boundCell = newCell(cell)

          boundCell.shell = None
          boundCell.sources = sources
          boundCell.target =
            cell.target match {
              case None => None
              case Some(tgt) => {
                val boundEdge = newEdge(tgt)
                boundEdge.incoming = Some(boundCell)
                Some(boundEdge)
              }
            }

          for { srcs <- sources } { srcs foreach (src => src.outgoing = Some(boundCell)) }

          boundCell
        }

      case Some(tree) =>
        {
          val boundCell = newCell(cell)

          def traverse(t : RoseTree[complex.CellType, Int]) : (RoseTree[CellType, Int], Option[EdgeType]) = {
            t match {
              case Rose(idx) =>
                {
                  val src = for { srcs <- sources } yield srcs(idx)
                  (Rose(idx), src)
                }
              case Branch(curCell, branches) =>
                {
                  val (newBranches, newSrcOpts) = (branches map (b => traverse(b))).unzip
                  val thisCell = generatePanelData(curCell, optSwitch(newSrcOpts))
                  thisCell.container = Some(boundCell)
                  (Branch(thisCell, newBranches), thisCell.target)
                }
            }
          }

          val (myShell, myTgt) = traverse(tree)

          boundCell.shell = Some(myShell)
          boundCell.target = myTgt
          boundCell.sources = sources

          boundCell
        }
    }
  }
}

