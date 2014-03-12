/**
  * GoalListCell.scala - A custom cell type for displaying goal markers
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.Includes._

import javafx.scene.{control => jfxsc}

import orchard.core._

class GoalListCell extends jfxsc.ListCell[GoalComplex] {

  val styleIndex = getStyleClass.length
  getStyleClass add "orch-list-cell"

  def setCellStyle(str : String) = 
    getStyleClass(styleIndex) = str

  override def updateItem(gm : GoalComplex, empty : Boolean) = {
    super.updateItem(gm, empty)

    if (! empty) {
      val v = gm.topCell.item.asInstanceOf[FreeVariable]

      if (v.variable.isThin) {
        setCellStyle("orch-list-cell-var-thin")
      } else {
        setCellStyle("orch-list-cell-var")
      }

      setText(v.toString)
    } else {
      setCellStyle("orch-list-cell")
    }
  }

}

