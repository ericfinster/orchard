/**
  * GalleryComplex.scala - A Complex which maintains a collection of panels
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.complex

import orchard.core.util._

trait GalleryComplex[A] extends CellComplex[A] {

  type CellType <: GalleryCell

  type PanelType <: Panel

  type PanelCellType <: PanelCell
  type PanelEdgeType <: PanelEdge

  trait Panel { thisPanel : PanelType =>

    def render : Unit
    def baseCell : PanelCellType

  }

  trait PanelCell extends CellBase[PanelCellType, PanelEdgeType] {
    thisPanelCell : PanelCellType =>

    def item : A = complexCell.item
    def complexCell : CellType

    // Because of unicity, we can actually implement these methods
    // on the panel cells here, meaning no initialization will be 
    // required for panels ...
    
    def canopy : Option[RoseTree[PanelCellType, Int]] = 
      for {
        complexCanopy <- complexCell.canopy
      } yield {
        complexCanopy map ((c => c.panelCell), (i => i))
      }

    def sources : Option[Vector[PanelEdgeType]] = 
      for {
        complexSources <- complexCell.sources
      } yield {
        complexSources map (_.panelEdge)
      }

    def target : Option[PanelEdgeType] =
      for {
        complexTarget <- complexCell.target
      } yield complexTarget.panelEdge

    def container : Option[PanelCellType] =
      for {
        complexContainer <- complexCell.container
      } yield complexContainer.panelCell

  }

  trait PanelEdge extends EdgeBase[PanelCellType, PanelEdgeType] {
    thisPanelEdge : PanelEdgeType =>

    def item : A = complexCell.item
    def complexCell : CellType

    def incoming : Option[PanelCellType] = 
      for {
        complexIncoming <- complexCell.incoming
      } yield complexIncoming.panelCell

    def outgoing : Option[PanelCellType] = 
      for {
        complexOutgoing <- complexCell.outgoing
      } yield complexOutgoing.panelCell

  }

  trait GalleryCell extends ComplexCell { thisCell : CellType =>

    def panelCell : PanelCellType
    def panelEdge : PanelEdgeType

  }

}
