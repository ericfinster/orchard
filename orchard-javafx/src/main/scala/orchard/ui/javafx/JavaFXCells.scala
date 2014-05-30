/**
  * JavaFXCells.scala - Cell implementations for lists and trees
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import javafx.scene.{control => jfxsc}

class ModuleTreeCell extends jfxsc.TreeCell[JavaFXModuleEntry] {

  getStyleClass add "orch-list-cell"

  override def updateItem(item : JavaFXModuleEntry, empty : Boolean) = {
    if (! empty) {
      setText(item.name)
    }
  }

}

class ModuleListCell extends jfxsc.ListCell[JavaFXModuleEntry] {

  getStyleClass add "orch-list-cell"

  override def updateItem(item : JavaFXModuleEntry, empty : Boolean) = {
    if (! empty) {
      setText(item.name)
    }
  }

}
