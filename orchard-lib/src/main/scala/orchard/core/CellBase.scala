/**
  * CellBase.scala - Base traits for mutable cells
  * 
  * @author Eric Finster
  * @version 0.1
  */

package orchard.core

import Util._

trait CellBase[C <: CellBase[C, E], E <: EdgeBase[C, E]] { thisCell : C =>

  var shell : Option[RoseTree[C, Int]] = None
  var target : Option[E] = None
  var sources : Option[List[E]] = None
  var container : Option[C] = None

  def isObject : Boolean = target == None
  def isExternal : Boolean = shell == None
  def isBase : Boolean = container == None
  def isLoop : Boolean = 
    sources match {
      case None => false
      case Some(srcs) =>
        target match {
          case None => false
          case Some(tgt) => {
            if (srcs.length == 1) {
              if (srcs.head == tgt) true else false
            } else false
          }
        }
    }

  def baseContainer : C =
    container match {
      case None => this
      case Some(c) => c.baseContainer
    }

  def foreachCell(action : C => Unit) : Unit = {
    action(this)

    shell match {
      case None => ()
      case Some(tree) => 
        tree.foreachCell(c => c foreachCell action)
    }
  }

  def edgeAt(ptr : RoseTree[C, Int]) : Option[E] = {
    // Given a pointer to a location in the shell, return its outgoing edge
    ptr match {
      case Rose(idx) => for { srcs <- sources } yield srcs(idx)
      case Branch(value, branches) => value.target
    }
  }

  //============================================================================================
  // MUTABILITY ROUTINES
  //

  // I would really like to split these off into a different trait ...

  // Reorganize the source lists with the help of the external cells
  // I don't like that the default comparison here is with a null.  You
  // could make it a flag somehow ...
  def comb(newEdge : E) : Unit = {
    // Pointless to comb an object
    if (isObject) return ()

    val curSources = sources.force

    shell match {
      case None => ()       // Assume external cells have their sources set correctly
      case Some(tree) =>
        {
          var curIdx = -1

          def rebuildShell(t : RoseTree[C, Int]) : (RoseTree[C, Int], List[E]) =
            t match {
              case Rose(idx) =>
                {
                  curIdx += 1
                  (Rose(curIdx), curSources(idx) :: Nil)
                }
              case Branch(cell, branches) =>
                {
                  // Pass to this cell first
                  cell.comb(newEdge)

                  val (newBranches, newSources) = 
                    (cell.sources.force map
                       (src => {
                          if (src == newEdge) {
                            curIdx += 1
                            (Rose(curIdx), newEdge :: Nil)
                          } else {
                            val thisBranch =
                              (branches find
                                 (b => edgeAt(b).force == src)).force("Lookup failed")

                            rebuildShell(thisBranch)
                          }
                        })).unzip

                  (Branch(cell, newBranches), newSources.flatten)
                }
            }

          val (newShell, newSources) = rebuildShell(tree)

          shell = Some(newShell)
          sources = Some(newSources)
        }
    }
  }

  def sprout(newEdge : E, newSources : List[E]) : Unit =
  {
    if (! isExternal)
      throw new IllegalArgumentException("Sprout called on non-external cell")

    newEdge.outgoing = Some(this.asInstanceOf[C])
    sources = Some(newSources)

    baseContainer.comb(newEdge)
  }

  def spawn(oldCell : C, newCell : C, newEdge : E,
            oldCellSources : List[E],
            newCellSources : List[E]) : RoseTree[C, Int]  = 
  {
    if (isExternal)
      throw new IllegalArgumentException("Spawn called on external cell")

    // Look up the old cell cell in my shell
    val ptr = RoseZipper(shell.force, Nil).lookup(oldCell).force("Lookup failed.")

    val newFocus = 
      ptr.focus match {
        case Branch(oc, branches) =>
          {
            // Return the new focus with the new cell in its correct place and the
            // branches adjusted accordingly ...

            val newCellBranches = newCellSources map
            (src => {
               val branch = (branches find (b => edgeAt(b).force == src)).force("No branch found for this edge.")
               edgeAt(branch).force.outgoing = Some(newCell)
               branch
             })

            val oldCellBranches = oldCellSources map
            (src => {
               if (src == newEdge) {
                 Branch(newCell, newCellBranches)
               } else {
                 (branches find (b => edgeAt(b).force == src)).force("No branch found for this edge.")
               }
             })

            Branch(oc, oldCellBranches)
          }
        case _ => throw new IllegalArgumentException("This can't happen.")
      }

    // The new shell should be set up now ...
    val newShell = ptr.setFocus(newFocus).zip
    shell = Some(newShell)

    // Set up the new cell and edge
    newEdge.incoming = Some(newCell)
    newEdge.outgoing = Some(oldCell)

    newCell.target = Some(newEdge)
    newCell.sources = Some(newCellSources)
    newCell.container = Some(this.asInstanceOf[C])

    oldCell.sources = Some(oldCellSources)

    // Reorganize the sources ...
    baseContainer.comb(null.asInstanceOf[E])

    newShell
  }

  def enclose(enclosingCell : C,
              location : RoseZipper[C, Int],
              selector : C => Boolean) 
      : (RoseTree[C, Int],          // The source tree of the filler
         RoseTree[C, Int]) =        // The source tree of the new cell
  {
    var localIndex : Int = 0

    def clip(focus : RoseTree[C, Int], selector : C => Boolean)
        : (RoseTree[C, Int], List[RoseTree[C, Int]]) =
    {
      focus match {
        case Rose(idx) =>
          {
            val subTree = Rose(localIndex)
            localIndex += 1

            (subTree, Rose(idx) :: Nil)
          }
        case Branch(value, branches) =>
          {
            if (selector(value)) {
              // This cell is part of the selection
              val (newBranches, newClippings) = (branches map (b => clip(b, selector))).unzip
              (Branch(value, newBranches), newClippings.flatten)
            } else {
              // This cell is not part of the selection (i.e. it's on the boundary)
              val subTree = Rose(localIndex)
              localIndex += 1

              (subTree, Branch(value, branches) :: Nil)
            }
          }
      }
    }

    val (subTree, clippings) = clip(location.focus, selector)

    subTree.foreach((cell => cell.container = Some(enclosingCell)), (_ => ()))

    val targetSources = optSwitch(clippings map (branch => edgeAt(branch)))
    val newShell = location.setFocus(Branch(enclosingCell, clippings)).zip

    enclosingCell.shell = Some(subTree)                      // OK: the selection
    enclosingCell.target = edgeAt(location.focus)            // OK: Should be the target of the tree
    enclosingCell.sources = targetSources                    // OK: Target of all clipped entities
    enclosingCell.container = Some(this.asInstanceOf[C])     // OK: it's contained in this cell

    // Update the shell of this cell.  All other fields are unchanged.
    shell = Some(newShell)

    (newShell, subTree)
  }

  def globEnclose(newTarget : C) : Unit = {
    newTarget.target = target
    newTarget.sources = sources

    newTarget.shell = 
      for { srcs <- sources }
      yield {
        var srcIndex = -1

        val branches = srcs map 
          (src => {
             srcIndex += 1
             Rose(srcIndex)
           })

        Branch(this.asInstanceOf[C], branches)
      }

    container = Some(newTarget)
  }
}

trait EdgeBase[C <: CellBase[C, E], E <: EdgeBase[C, E]] { thisEdge : E =>

  var incoming : Option[C] = None
  var outgoing : Option[C] = None

  def foreachEdge(action : E => Unit) : Unit = {
    for { cell <- incoming } {
      for { srcs <- cell.sources } {
        srcs foreach (src => src foreachEdge action)
      }
    }

    action(this)
  }
}
