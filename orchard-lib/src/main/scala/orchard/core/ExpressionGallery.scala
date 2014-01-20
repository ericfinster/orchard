/**
  * ExpressionGallery.scala - A Gallery for displaying expressions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

import Util._

trait ExpressionGallery extends Gallery[Polarity[Option[Expression]]] { 

  override type PanelType <: ExpressionPanel

  //============================================================================================
  // SEMANTICS
  //

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
        val zipper = new RoseZipper(positiveBase.canopy.force, Nil)

        val basePtr = positiveBase.canopy.force match {
          case Rose(_) => throw new IllegalArgumentException("Negative cell has no sources.")
          case Branch(negCell, brs) => {
            val i = brs indexWhere
              (branch =>
                branch match {
                  case Rose(idx) => {
                    val srcs = positiveBase.sources.force
                    if (srcs(idx) == base.owner) true else false
                  }
                  case Branch(cell, _) => {
                    if (cell.target.force == base.owner) true else false
                  }
                })

            zipper.visitBranch(i).force
          }
        }

        positiveBase.insertComposite(Neutral(compositeExpr), Neutral(fillerExpr), basePtr, (_ => false))
      }
    }

}
