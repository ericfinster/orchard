/**
  * JavaFXCells.scala - Cell implementations for lists and trees
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scala.collection.JavaConversions._

import javafx.scene.{control => jfxsc}

trait JavaFXCell { thisCell : jfxsc.Cell[JavaFXModuleEntry] => 

  getStyleClass add "orch-list-cell"
  val cellStyleIndex = getStyleClass.length
  getStyleClass add "orch-list-cell-unknown"

  def setCellStyle(style : String) = getStyleClass(cellStyleIndex) = "orch-list-cell-" ++ style
  def removeCellStyle = setCellStyle("orch-list-cell-unknown")

  def renderCell(item : JavaFXModuleEntry) {
    item match {
      case mv : JavaFXModuleVariable => setCellStyle("var")
      case _ => removeCellStyle
    }

    setText(item.name)
  }
}

class ModuleTreeCell extends jfxsc.TreeCell[JavaFXModuleEntry] with JavaFXCell {

  override def updateItem(item : JavaFXModuleEntry, empty : Boolean) = {
    super.updateItem(item, empty)

    if (! empty)
      renderCell(item)
  }

}

class ModuleListCell extends jfxsc.ListCell[JavaFXModuleEntry] with JavaFXCell {

  override def updateItem(item : JavaFXModuleEntry, empty : Boolean) = {
    super.updateItem(item, empty)

    if (! empty)
      renderCell(item)
  }

}
