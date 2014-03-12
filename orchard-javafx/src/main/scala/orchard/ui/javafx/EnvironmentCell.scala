/**
  * EnvironmentCell.scala - A customized ListCell for displaying expressions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import javafx.scene.{control => jfxsc}

import orchard.core._

class EnvironmentCell extends jfxsc.ListCell[NCell[Expression]] {

  getStyleClass add "orch-list-cell"

  var lastStyle : Option[String] = None

  def setStyleType(styleType : String) = {
    lastStyle foreach (st => getStyleClass remove st)
    getStyleClass add styleType
    lastStyle = Some(styleType)
  }

  override def updateItem(expr : NCell[Expression], empty : Boolean) = {
    super.updateItem(expr, empty)

    if (! empty) {
      // Set the style based on the semantics ...
      expr.value match {
        case Variable(_, isThin) => {
          if (isThin) {
            setStyleType("orch-list-cell-var-thin")
          } else {
            setStyleType("orch-list-cell-var")
          }
        }
        case Filler(_) => setStyleType("orch-list-cell-filler")
        case FillerFace(_, _, isThin) => {
          if (isThin) {
            setStyleType("orch-list-cell-filler-face-thin")
          } else {
            setStyleType("orch-list-cell-filler-face")
          }
        }
        case UnicityFiller(_) => setStyleType("orch-list-cell-ufiller")
        case Application(_, _, _) => setStyleType("orch-list-cell-app")
      }

      setText(expr.toString)
    }
  }
}
