/**
  * BorderedTitledPane.scala - A simple titled and bordered pane
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx.controls

import scalafx.Includes._

import javafx.{scene => jfxs}
import javafx.{geometry => jfxg}
import javafx.scene.{layout => jfxsl}

import scalafx.scene.control.Label
import scalafx.scene.layout.StackPane

class BorderedTitledPane(title : String, contentNode : jfxs.Node) extends jfxsl.StackPane {

  val label : Label = new Label(" " ++ title ++ " ")  {
    styleClass += "orch-bordered-titled-title"
  }

  jfxsl.StackPane.setAlignment(label, jfxg.Pos.TOP_CENTER)

  val contentPane = new StackPane {
    content = contentNode
  }

  contentNode.getStyleClass add "orch-bordered-titled-content"

  getChildren add label
  getChildren add contentPane

  getStyleClass add "orch-bordered-titled-border"

}
