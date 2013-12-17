/**
  * Panel.scala - A collection of cells and edges
  * 
  * @author Eric Finster
  * @version 0.1
  */

package orchard.core

import Util._

trait Panel[A] extends EventConduit[CellEvent] { thisPanel =>

  type CellType <: PanelCell
  type EdgeType <: PanelEdge

  type ComplexType <: MutableComplex[A]

  val complex : ComplexType

  var baseCell : CellType

  def newCell(owner : complex.CellType) : CellType
  def newEdge(owner : complex.CellType) : EdgeType

  def refresh : Unit

  //============================================================================================
  // CELL AND EDGE TRAITS
  //

  trait PanelCell extends MutableCellBase[CellType, EdgeType] with EventConduit[CellEvent] { thisCell : CellType =>

    def owner : complex.CellType

    def item : A = owner.item

    var canopy : Option[RoseTree[CellType, Int]] = None
    var target : Option[EdgeType] = None
    var sources : Option[Vector[EdgeType]] = None
    var container : Option[CellType] = None

    reactTo(owner)

    // These don't get used in the loop above, but they may be useful sometime
    def refreshCellData : Unit = {
      target = for { tgt <- owner.target } yield { tgt.getOrCreateEdge(thisPanel) }
      sources = for { srcs <- owner.sources } yield { srcs map (src => src.getOrCreateEdge(thisPanel)) }
      canopy = for { cnpy <- owner.canopy } yield { cnpy.map((c => c.getOrCreateCell(thisPanel)), (i => i)) }
      container = for { cntr <- owner.container } yield { cntr.getOrCreateCell(thisPanel) }
    }

  }

  trait PanelEdge extends MutableEdgeBase[CellType, EdgeType] with EventConduit[CellEvent] { thisEdge : EdgeType =>

    def owner : complex.CellType

    def item : A = owner.item

    var incoming : Option[CellType] = None
    var outgoing : Option[CellType] = None

    reactTo(owner)

    def refreshEdgeData : Unit = {
      incoming = for { inc <- owner.incoming } yield { inc.getOrCreateCell(thisPanel) }
      outgoing = for { otg <- owner.outgoing } yield { otg.getOrCreateCell(thisPanel) }
    }

  }

  //============================================================================================
  // PANEL REFRESHING AND GENERATION
  //

  def refreshPanelData : Unit = {

    def refreshLoop(cell : CellType) : Unit = {

      val newCanopy = cell.owner.canopy match {
        case None => {
          // We update the edges at the external nodes
          cell.target = 
            for { tgt <- cell.owner.target }
            yield {
              val theTargetEdge = tgt.getOrCreateEdge(thisPanel) 

              theTargetEdge.incoming = for { inc <- theTargetEdge.owner.incoming } yield { inc.getOrCreateCell(thisPanel) }
              theTargetEdge.outgoing = for { otg <- theTargetEdge.owner.outgoing } yield { otg.getOrCreateCell(thisPanel) }

              theTargetEdge
            }

          cell.sources = 
            for { srcs <- cell.owner.sources }
            yield {
              srcs map (src => {
                val theSourceEdge = src.getOrCreateEdge(thisPanel)

                theSourceEdge.incoming = for { inc <- theSourceEdge.owner.incoming } yield { inc.getOrCreateCell(thisPanel) }
                theSourceEdge.outgoing = for { otg <- theSourceEdge.owner.outgoing } yield { otg.getOrCreateCell(thisPanel) }

                theSourceEdge
              })
            }

          None
        }
        case Some(tree) => {

          val newTree = tree.map((c => { val theCell = c.getOrCreateCell(thisPanel) ; refreshLoop(theCell) ; theCell }), (i => i))

          cell.target = for { tgt <- cell.owner.target } yield { tgt.getOrCreateEdge(thisPanel) }
          cell.sources = for { srcs <- cell.owner.sources } yield { srcs map (src => src.getOrCreateEdge(thisPanel)) }

          Some(newTree)
        }
      }

      cell.canopy = newCanopy
      cell.container = for { cntr <- cell.owner.container } yield { cntr.getOrCreateCell(thisPanel) }
    }

    refreshLoop(baseCell)
  }

}

