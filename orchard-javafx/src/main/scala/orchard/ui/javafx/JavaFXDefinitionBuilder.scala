/**
  * JavaFXDefinitionBuilder.scala - JavaFX implementation of a definiton builder
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.Includes._

import scalafx.scene.layout.StackPane

import scalafx.geometry.Side
import scalafx.geometry.Insets
import scalafx.geometry.Orientation

import scalafx.scene.control.Tab
import scalafx.scene.control.TabPane
import scalafx.scene.control.ListView
import scalafx.scene.control.SplitPane
import scalafx.scene.control.TitledPane
import scalafx.scene.control.Accordion

import scalafx.collections.ObservableBuffer

import javafx.scene.{control => jfxsc}

import orchard.core._

class JavaFXDefinitionBuilder extends Tab with DefinitionBuilder {

  val environment = ObservableBuffer.empty[NCell[Expression]]

  var truncationLevel : Int = 0
  var contractibilityLevel : Int = 0

  val sheetPane = new StackPane

  val controlAccordion = new Accordion

  val envPane = new TitledPane {
    text = "Environment"
    content = new ListView[NCell[Expression]] {
      items = environment
      cellFactory = (_ => new EnvironmentCell)
    }
  }

  val substPane = new TitledPane {
    text = "Substitutions"
  }

  val shtPane = new TitledPane {
    text = "Sheets"
  }

  controlAccordion.panes = List(envPane, substPane, shtPane)

  val previewerPane = new StackPane {
    styleClass += "orch-pane"
  }

  val horizontalSplit = new SplitPane {
    orientation = Orientation.HORIZONTAL
    dividerPositions = 0.8f
  }

  horizontalSplit.items.addAll(
    (new StackPane { content = sheetPane; padding = Insets(10, 10, 10, 10); styleClass += "orch-pane" }).delegate,
    (new StackPane { content = controlAccordion; padding = Insets(10, 10, 10, 10); styleClass += "orch-pane" }).delegate
  )

  val verticalSplit = new SplitPane {
    orientation = Orientation.VERTICAL
    dividerPositions = 0.8f
  }

  verticalSplit.items.addAll(horizontalSplit, previewerPane)
  content = verticalSplit

  // newSheet

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
          case Filler(_, _) => setStyleType("orch-list-cell-filler")
          case FillerTarget(_, _, isThin) => {
            if (isThin) {
              setStyleType("orch-list-cell-filler-tgt-thin")
            } else {
              setStyleType("orch-list-cell-filler-tgt")
            }
          }
        }

        setText(expr.toString)
      }
    }

    // setOnMouseClicked(new EventHandler[MouseEvent] {
    //   def handle(ev : MouseEvent) {
    //     if (! isEmpty) {
    //       setPreview(getItem)

    //       if (ev.getClickCount > 1) {
    //         newBuilder(getItem map (e => Some(e)))
    //       }
    //     }
    //   }
    // })

  }

  var tabCount = 1

  // def newSheet : Unit = newSheet(Object(None))

  // def newSheet(seed : NCell[Option[Expression]]) = {
  //   val builder = new ExpressionBuilder(CardinalComplex(seed))
  //   val newTab = new Tab { text = "Sheet " ++ tabCount.toString ; content = builder }
  //   tabCount += 1
  //   sheetPane += newTab
  //   sheetPane.selectionModel().select(newTab)
  //   // reactTo(builder)
  //   builder.renderAll
  // }

}
