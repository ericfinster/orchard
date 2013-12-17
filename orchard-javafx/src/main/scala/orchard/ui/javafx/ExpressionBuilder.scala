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

class ExpressionBuilder(seed : NCell[Polarity[Option[Expression]]]) extends JavaFXGallery[Polarity[Option[Expression]]] { thisBuilder =>

  def this() = this(Composite(Negative, Seed(Object(Neutral(None))), Positive))

  //============================================================================================
  // INITIALIZATION
  //

  type PanelType = ExpressionBuilderPanel

  val complex = new ExpressionBuilderComplex(seed)

  reactTo(complex)

  def newPanel(i : Int) : ExpressionBuilderPanel = {
    val panel = new ExpressionBuilderPanel(complex, i)
    reactTo(panel) 
    panel 
  }

  initialize

  var lastComposite : GalleryCell = null
  var lastFiller : GalleryCell = null

  //============================================================================================
  // EVENTS
  //

  override def onEventEmitted(ev : CellEvent) = {
    ev match {

      case ComplexExtended => {
        this(complex.dimension - 1).refresh
        val extPanel = newPanel(complex.dimension)
        appendPanel(extPanel)
        extPanel.render
        fastForward
      }

      case CellClicked(c) => {
        val cell = c.asInstanceOf[GalleryCell]

        if (cell.owner.isNeutral)
          clearAndSelect(cell)
        else {
          c.owner.dumpInfo
          deselectAll
        }
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

      case complex.ChangeEvents.CompositeInsertionEvent(c, u) => {
        println("Handling an insertion event ...")

        val dim = c.dimension

        println("Composite is in dimension " ++ dim.toString)

        val compPanel = thisBuilder(dim)
        val univPanel = thisBuilder(dim + 1)

        println("Found the panels")

        lastComposite = c.cellOnPanel(compPanel)
        lastFiller = u.cellOnPanel(univPanel)

        println("Set the last guys ..")

        val affectedDimensions = Range(dim, complex.dimension + 1)

        println("Going to refresh dimensions: " ++ affectedDimensions.toString)

        affectedDimensions foreach (i => panels(i).refresh)
      }

      // This makes the inserted cells available for inspection later ...
      // case Enclose(cell) => { lastComposite = cell.asInstanceOf[GalleryCell] }
      // case Spawn(cell) => { lastFiller = cell.asInstanceOf[GalleryCell] }

      case _ => super.onEventEmitted(ev)
    }
  }

  //============================================================================================
  // SEMANTICS
  //

  def emptyComposition = composeSelection(None, None)

  def composeSelection(compositeExpr : Option[Expression], fillerExpr : Option[Expression]) = 
    selectionBase match {
      case None => ()
      case Some(base) => {
        if (base.owner.dimension >= dimension - 1)
          complex.extend

        // I think the casting is unnecessary: you know that you're looking
        // for the positive container, which you can get by hand ... see below
        val baseContainer = 
          base.owner.container.force.asInstanceOf[complex.CellType]

        val basePtr = (new RoseZipper(baseContainer.canopy.force, Nil))
          .lookup(base.owner.asInstanceOf[complex.CellType])
          .force("Lookup failed for selection base")

        val owners = selectedCells map (_.owner.asInstanceOf[complex.CellType])

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
