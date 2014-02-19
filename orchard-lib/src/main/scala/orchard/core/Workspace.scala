/**
  * Workspace.scala - A workspace consisting of an environment and a collection of sheets
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.mutable.Buffer

trait Workspace extends Environment {

  type GalleryType <: ExpressionGallery

  def name : String

  def stabilityLevel : Option[Int]
  def invertibilityLevel : Option[Int]
  def unicityLevel : Option[Int]

  var activeSheet : Option[GalleryType] = None

  val sheets = Buffer.empty[GalleryType]
  val environment = Buffer.empty[NCell[Expression]]

  def addToEnvironment(expr : NCell[Expression]) = environment += expr

  def assume(id : String, isThin : Boolean) = {
    for {
      sheet <- activeSheet
      emptyCell <- sheet.selectionBase
    } {
      if (emptyCell.owner.isShell) {
        sheet.deselectAll
        emptyCell.owner.item = Neutral(Some(Variable(id, isThin)))

        // To update the highlighting ...
        sheet.refreshAll

        val exprCell : NCell[Expression] = emptyCell.owner.getSimpleFramework.toExpressionCell
        addToEnvironment(exprCell)

        sheet.selectAsBase(emptyCell)
      }
    }
  }

  def fillExposedNook(targetIdent : Identifier, fillerIdent : Identifier) = {
    for {
      sheet <- activeSheet
      nookCell <- sheet.selectionBase
    } {
      val complex = sheet.complex

      sheet.deselectAll

      val filler = Filler(fillerIdent)
      nookCell.owner.item = Neutral(Some(filler))

      if (nookCell.owner.isOutNook) {
        val targetCell = nookCell.owner.target.get
        val targetIsThin = (true /: (nookCell.owner.sources.get map (_.isThin))) (_&&_)

        targetCell.item = Neutral(Some(FillerFace(targetIdent, filler.id, targetIsThin)))
      } else {
        val targetCell = nookCell.owner.emptySources.head
        val targetIsThin = nookCell.owner.target.get.isThin
        
        targetCell.item = Neutral(Some(FillerFace(targetIdent, filler.id, targetIsThin)))
        addToEnvironment(targetCell.getSimpleFramework.toExpressionCell)
      }

      addToEnvironment(nookCell.owner.getSimpleFramework.toExpressionCell)
    }
  }

  def fillFromEnvironment(emptyCell : GalleryType#GalleryCell, expr : NCell[Expression]) = {
    for {
      sheet <- activeSheet
    } {
      val complex = sheet.complex

      emptyCell.owner.skeleton.asInstanceOf[NCell[complex.ExpressionBuilderCell]]
        .zip(expr) match {
        case None => println("Not compatible. Zip failed.")
        case Some(zippedTree) => {

          var itFits = true

          zippedTree map (pr => {
            val (eCell, e) = pr

            eCell.item match {
              case Neutral(None) => ()
              case Neutral(Some(f)) => if (itFits) { itFits &&= (e == f) } else ()
              case _ => itFits = false
            }
          })

          if (itFits) {
            sheet.deselectAll

            zippedTree map (pr => {
              val (eCell, e) = pr

              // This is overkill
              eCell.item = Neutral(Some(e))
            })
          } else {
            println("Cell does not fit.")
          }
        }
      }

      // Overkill!!!
      sheet.refreshAll
    }
  }

}
