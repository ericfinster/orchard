/**
  * ExpressionGallery.scala - A Gallery for displaying expressions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

import Util._

trait ExpressionGallery extends SelectableGallery[Polarity[Option[Expression]]] { 

  override type PanelType <: ExpressionPanel

  var lastComposite : GalleryCell
  var lastFiller : GalleryCell

  //============================================================================================
  // SEMANTICS
  //

  def selectionIsComposable : Boolean = {
    val cellsAreComplete = (true /: (selectedCells map (_.owner.isComplete))) (_&&_)
    cellsAreComplete && selectionIsExtrudable
  }

  def selectionIsShell : Boolean = {
    selectionBase match {
      case None => false
      case Some(cell) => cell.owner.isShell
    }
  }

  def selectionIsEmptyCell : Boolean = {
    selectionBase match {
      case None => false
      case Some(cell) => cell.owner.isEmpty
    }
  }

  def selectionIsExtrudable : Boolean = {
    selectionBase match {
      case None => false
      case Some(cell) => {
        cell.container match {
          case None => false
          case Some(cont) => cont.owner.isPolarized
        }
      }
    }
  }

  def selectionIsDroppable : Boolean = {
    selectionBase match {
      case None => false
      case Some(cell) => {
        cell.owner.outgoing match {
          case None => false  // This shouldn't happen
          case Some(o) => {
            if (o.isPolarized) true else {
              o.container match {
                case None => false
                case Some(cont) => cont.isPolarized
              }
            }
          }
        }
      }
    }
  }

  def extrudeSelection = {
    if (selectionIsExtrudable) {
      emptyComposition
      clearAndSelect(lastComposite)
    }
  }

  def extrudeDrop = {
    if (selectionIsDroppable) {
      emptyDrop
      clearAndSelect(selectionBase.get)
    }
  }

  def emptyComposition = composeSelection(None, None)

  def composeSelection(compositeExpr : Option[Expression], fillerExpr : Option[Expression]) = 
    selectionBase match {
      case None => ()
      case Some(base) => {
        val cmplx = complex

        if (base.owner.dimension >= dimension - 1)
          cmplx.extend

        // I think the casting is unnecessary: you know that you're looking
        // for the positive container, which you can get by hand ... see below
        val baseContainer = 
          base.owner.container.force.asInstanceOf[cmplx.CellType]

        val basePtr = (new RoseZipper(baseContainer.canopy.force, Nil))
          .lookup(base.owner.asInstanceOf[cmplx.CellType])
          .force("Lookup failed for selection base")

        val owners = selectedCells map (_.owner.asInstanceOf[cmplx.CellType])

        baseContainer.insertComposite(Neutral(compositeExpr), Neutral(fillerExpr), basePtr, (cell => owners contains cell))
      }
    }

  def emptyDrop = dropAtSelection(None, None)

  def dropAtSelection(compositeExpr : Option[Expression], fillerExpr : Option[Expression]) =
    selectionBase match {
      case None => ()
      case Some(base) => {
        if (base.owner.dimension == dimension - 2) {
          complex.extend
        } else if (base.owner.dimension == dimension - 1) {
          complex.extend ; complex.extend
        }

        val activePanel = panels(base.owner.dimension + 1)
        val positiveBase = activePanel.baseCell.owner

        val outPtr = 
          new RoseZipper(positiveBase.canopy.get, Nil).
            lookup(base.owner.outgoing.get.asInstanceOf[activePanel.complex.CellType]).get

        val basePtr = outPtr.focus match {
          case Rose(_) => throw new IllegalArgumentException("Didn't find the outgoing cell!")
          case Branch(outCell, brs) => {
            val i = brs indexWhere
              (branch =>
                branch match {
                  case Rose(idx) => {
                    val srcs = positiveBase.sources.get
                    if (srcs(idx) == base.owner) true else false
                  }
                  case Branch(cell, _) => {
                    if (cell.target.get == base.owner) true else false
                  }
                })

            outPtr.visitBranch(i).get
          }
        }

        positiveBase.insertComposite(Neutral(compositeExpr), Neutral(fillerExpr), basePtr, (_ => false))
      }
    }

}
