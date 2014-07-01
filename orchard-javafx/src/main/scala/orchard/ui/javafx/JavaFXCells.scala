/**
  * JavaFXCells.scala - Cell implementations for lists and trees
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scala.collection.JavaConversions._

import javafx.scene.{control => jfxsc}

import orchard.core.ui.Stylable
import orchard.core.expression._

trait JavaFXCell[A <: Stylable] { thisCell : jfxsc.Cell[A] => 

  getStyleClass add "orch-list-cell"
  val cellStyleIndex = getStyleClass.length
  getStyleClass add "orch-list-cell-unknown"

  def setCellStyle(style : String) = getStyleClass(cellStyleIndex) = "orch-list-cell-" ++ style
  def removeCellStyle = setCellStyle("orch-list-cell-unknown")

  def renderCell(item : A) {
    setCellStyle(item.styleString)
    setText(item.name)
  }

}

class ModuleTreeCell extends jfxsc.TreeCell[ModuleItem] with JavaFXCell[ModuleItem] {
  override def updateItem(item : ModuleItem, empty : Boolean) = {
    super.updateItem(item, empty)

    if (! empty)
      renderCell(item)
  }
}

class ModuleListCell extends jfxsc.ListCell[ModuleItem] with JavaFXCell[ModuleItem] {
  override def updateItem(item : ModuleItem, empty : Boolean) = {
    super.updateItem(item, empty)

    if (! empty)
      renderCell(item)
  }
}
