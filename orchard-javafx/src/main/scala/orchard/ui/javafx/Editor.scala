/**
  * Editor.scala - Main module for JavaFX Orchard Editor
  * 
  * @author Eric Finster
  * @version 0.1
  */

package orchard.ui.javafx

import scalafx.Includes._
import scalafx.scene.Scene
import scalafx.application.JFXApp

import orchard.core._

object Editor extends JFXApp {

  val orchardScene = new Scene {
      root = new CardinalEditor
    }

  orchardScene.getStylesheets.add("orchard/ui/javafx/OrchardUI.css")

  stage = new JFXApp.PrimaryStage {
      title = "Orchard"
      width = 1500
      height = 500
      scene = orchardScene
    }

}


// val splitPane = new SplitPane {
//   orientation = Orientation.VERTICAL
//   dividerPositions = 0.6f
// }

// splitPane.getItems.addAll(borderPane, cellLibrary)

// val examples : ObservableBuffer[NCell[String]] =
//   ObservableBuffer(Example.Psi, Example1.w map (_.toString))

// val cellLibrary = new ListView[NCell[String]] {
//   items = examples
//   cellFactory = (lv => new NCellCell)
// }

// class NCellCell extends ListCell[NCell[String]] {
//   override protected def updateItem(item : NCell[String], empty : Boolean) = {
//     super.updateItem(item, empty)

//     if (item != null) {
//       val gallery = new SimpleGallery(item)
//       gallery.setMaxWidth(600)
//       gallery.setMaxHeight(100)
//       gallery.renderAll

//       setGraphic(gallery)
//     }
//   }
// }
