/**
  * ExpressionBuilder.scala
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

import orchard.core._
import Util._

class ExpressionBuilder extends JavaFXGallery[Polarity[Option[Expression]]] {

  //============================================================================================
  // INITIALIZATION
  //

  type PanelType = ExpressionBuilderPanel

  val complex = new ExpressionBuilderComplex(Composite(Negative, Seed(Object(Neutral(None))), Positive))

  def newPanel(i : Int) : ExpressionBuilderPanel = {
    val panel = new ExpressionBuilderPanel(complex, i)
    reactTo(panel) 
    panel 
  }

  initialize

  //============================================================================================
  // EVENTS
  //

  override def onEventEmitted(ev : CellEvent) = {
    ev match {

      case ComplexExtended => {
        val extPanel = newPanel(complex.baseCells.length - 1)
        appendPanel(extPanel)
        extPanel.render
        fastForward
      }

      case CellClicked(c) => {
        val cell = c.asInstanceOf[GalleryCell]

        if (cell.owner.isNeutral)
          clearAndSelect(cell)
        else
          deselectAll
      }

      case CellCtrlClicked(c) => {
        val cell = c.asInstanceOf[GalleryCell]

        selectionBase match {
          case None => if (cell.owner.isNeutral) selectAsBase(cell)
          case Some(base) => {
            if (cell != base) {
              if (cell.owner.isPolarized) {
                deselectAll
              } else {
                if (!trySelect(cell)) clearAndSelect(cell)
              }
            }
          }
        }
      }

      case _ => super.onEventEmitted(ev)
    }
  }

  //============================================================================================
  // SEMANTICS
  //

  def emptyComposition = 
    selectionBase match {
      case None => ()
      case Some(base) => {
        if (base.owner.dimension >= dimension - 1)
          complex.extend

        // I think the casting is unnecessary: you know that you're looking
        // for the positive container, which you can get by hand ... see below
        val baseContainer = 
          base.owner.container.force.asInstanceOf[complex.CellType]

        val basePtr = (new RoseZipper(baseContainer.shell.force, Nil))
          .lookup(base.owner.asInstanceOf[complex.CellType])
          .force("Lookup failed for selection base")

        val owners = selectedCells map (_.owner.asInstanceOf[complex.CellType])

        baseContainer.insertComposite(Neutral(None), Neutral(None), basePtr, (cell => owners contains cell))
      }
    }

  def emptyDrop = 
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
        val zipper = new RoseZipper(positiveBase.shell.force, Nil)

        val basePtr = positiveBase.shell.force match {
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

        positiveBase.insertComposite(Neutral(None), Neutral(None), basePtr, (_ => false))
      }
    }

}
