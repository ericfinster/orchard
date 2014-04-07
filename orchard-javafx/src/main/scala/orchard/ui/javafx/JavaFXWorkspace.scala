/**
  * JavaFXWorkspace.scala - JavaFX implementation of a workspace
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.Includes._

import scalafx.geometry._

import scalafx.scene.layout._
import scalafx.scene.control._

import orchard.core.ui._
import orchard.core.cell._
import orchard.core.editor._
import orchard.core.complex._
import orchard.core.expression._

class JavaFXWorkspace(
  val editor : JavaFXEditor,
  val name : String,
  val stabilityLevel : Option[Int],
  val invertibilityLevel : Option[Int],
  val unicityLevel : Option[Int]
) extends Workspace with JavaFXWorksheetEnv {

  var activeExpression : Option[NCell[Expression]] = None
  var activeGallery : Option[WorksheetGallery] = None 

  var sheetCount : Int = 1

  def activeSheet : Option[Worksheet] =
    for {
      gallery <- activeGallery
    } yield gallery.complex

  val sheetTabPane = new TabPane {
    side = Side.TOP
  }

  def newSheet = newSheet(CardinalComplex(Object(None)))

  def newSheet(seed : NCell[Polarity[Option[Expression]]]) : Unit = {
    val gallery = new WorksheetGallery(seed)

    val tab = new Tab {
      text = "Sheet " ++ sheetCount.toString
      content = gallery

      onClosed = () => {
        sheets -= gallery.complex
      }

      onSelectionChanged = () => {
        if (selected())
          activeGallery = Some(gallery)
      }
    }

    sheetTabPane += tab
    sheetTabPane.getSelectionModel.select(tab)
    sheets += gallery.complex
    sheetCount += 1
    gallery.refreshAll
  }

}
