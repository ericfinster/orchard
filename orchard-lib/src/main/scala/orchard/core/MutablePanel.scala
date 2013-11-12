/**
  * MutablePanel.scala - A Panel which reacts to mutability events
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

import Util._

trait MutablePanel[A] extends Panel[A] {

  override type CellType <: MutablePanelCell
  override type EdgeType <: MutablePanelEdge

  override type ComplexType <: MutableComplex[A]

  def refresh : Unit
  def baseCell_=(bc : CellType) : Unit

  trait MutablePanelCell extends PanelCell { thisCell : CellType =>

    //============================================================================================
    // MUTABILITY EVENT HANDLER
    //

    import complex.ChangeEvents._

    def onCellChangeEvent(ev : ChangeEvent) = {
      ev match {
        case SproutEvent(sproutedEdge, newSources) => {
          val mySources = sources.force
          val newVisualEdge = newEdge(sproutedEdge)
          val newVisualSources =
            newSources map
              (src =>
                if (src == sproutedEdge) {
                  newVisualEdge
                } else {
                  (mySources find (s => s.owner == src)).force("Edge lookup failed")
                })

          sprout(newVisualEdge, newVisualSources)
          refresh
        }

        case SpawnEvent(oldCell, spawnedCell, spawnedEdge, oldCellSrcs, newCellSrcs) => {
          // Create the new visual cell and edge
          val newVisualCell = newCell(spawnedCell)
          val newVisualEdge = newEdge(spawnedEdge)

          // Now find the old cell
          val ptr : RoseZipper[CellType, Int] =
            RoseZipper(shell.force, Nil).find(cell => cell.owner == oldCell).force("Could not find the old cell!")

          val oldVisualCell = ptr.focus.rootElement.force
          val ovSources = oldVisualCell.sources.force

          val newVisualCellSources : List[EdgeType] = newCellSrcs map
            (src => (ovSources find (s => s.owner == src)).force("Edge lookup failed"))

          val oldVisualCellSources : List[EdgeType] = oldCellSrcs map
            (src =>
              if (src == spawnedEdge)
                newVisualEdge
              else
                (ovSources find (s => s.owner == src)).force("Edge lookup failed")
            )

          // This will set the semantic properties and then comb the tree
          spawn(oldVisualCell, newVisualCell, newVisualEdge, oldVisualCellSources, newVisualCellSources)
          refresh
        }

        case EncloseEvent(enclosingCell, location, selector) => {
          // Make a new cell and then call my own enclose method with the appropriate data.
          val myLocation = new RoseZipper[CellType, Int](shell.force, Nil).seek(location.toAddr).force
          val mySelector : CellType => Boolean = (c => selector(c.owner))
          val newPanelCell : CellType = newCell(enclosingCell)

          val (fillerSources, universalSources) = enclose(newPanelCell, myLocation, mySelector)
          refresh
        }

        case GlobEncloseEvent(newTarget) => {
          val newPanelTarget = newCell(newTarget)
          globEnclose(newPanelTarget)
          baseCell = newPanelTarget
          refresh
        }

        case _ => ()
      }
    }
  }

  trait MutablePanelEdge extends PanelEdge { thisEdge : EdgeType =>
  }

}
