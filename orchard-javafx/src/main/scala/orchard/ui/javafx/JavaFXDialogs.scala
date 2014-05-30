/**
  * JavaFXDialogs.scala - Dialog Implementations for Orchard
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.Includes._
import scalafx.scene.layout._
import scalafx.scene.control._
import scalafx.geometry._

import controls._

trait JavaFXDialogs { thisEditor : JavaFXEditor =>

  object NewModuleDialog extends CancellableDialog {

    heading.text = "New Module"

    val nameLabel = new Label("Name: ")
    val nameField = new TextField { promptText = "Module name" }
    val nameHBox = new HBox {
      content = List(nameLabel, nameField)
      alignment = Pos.CENTER
      spacing = 5
      padding = Insets(0, 10, 10, 10)
    }

    GridPane.setColumnSpan(nameHBox, 2)

    val stableButton = new RadioButton("Stable") {
      onAction = { () =>
        stabilityField.text = "infty"
        stabilitySlider.disable = true
        stabilityField.disable = true
      }
    }

    val unstableButton = new RadioButton("Unstable") {
      onAction = { () =>
        stabilityField.disable = false
        stabilitySlider.disable = false
        stabilityField.text = stabilitySlider.value().toInt.toString
      }
    }

    val stabilityToggle = new ToggleGroup {
      toggles = List(stableButton, unstableButton)
    }

    val stabilitySlider = new Slider {
      min = 0
      max = 10
      majorTickUnit = 1
      minorTickCount = 0
      snapToTicks = true
      showTickMarks = false
    }

    stabilitySlider.value onChange {
      stabilityField.text = stabilitySlider.value().toInt.toString
    }

    val stabilityField = new TextField { editable = false }

    val noInvertibilityButton = new RadioButton("No Invertibility") {
      onAction = { () =>  ()
        noUnicityButton.fire
        invertibilityField.text = "infty"
        invertibilitySlider.disable = true
        invertibilityField.disable = true
      }
    }

    val finiteInvertibilityButton = new RadioButton("Finite Invertibility") {
      onAction = { () => ()
        invertibilityField.disable = false
        invertibilitySlider.disable = false
        invertibilityField.text = invertibilitySlider.value().toInt.toString
      }
    }

    val invertibilityToggle = new ToggleGroup {
      toggles = List(noInvertibilityButton, finiteInvertibilityButton)
    }

    val invertibilitySlider = new Slider {
      min = -1
      max = 10
      majorTickUnit = 1
      minorTickCount = 0
      snapToTicks = true
      showTickMarks = false
    }

    invertibilitySlider.value onChange {
      invertibilityField.text = invertibilitySlider.value().toInt.toString
    }

    val invertibilityField = new TextField { editable = false }

    val noUnicityButton = new RadioButton("No Unicity") {
      onAction = { () =>  ()
        unicityField.text = "infty"
        unicitySlider.disable = true
        unicityField.disable = true
      }
    }

    val finiteUnicityButton = new RadioButton("Finite Unicity") {
      onAction = { () => ()
        finiteInvertibilityButton.fire
        invertibilitySlider.max = unicitySlider.value()
        unicityField.disable = false
        unicitySlider.disable = false
        unicityField.text = unicitySlider.value().toInt.toString
      }
    }

    val unicityToggle = new ToggleGroup {
      toggles = List(noUnicityButton, finiteUnicityButton)
    }

    val unicitySlider = new Slider {
      min = 0
      max = 10
      majorTickUnit = 1
      minorTickCount = 0
      snapToTicks = true
      showTickMarks = false
    }

    unicitySlider.value onChange {
      invertibilitySlider.max = unicitySlider.value()
      unicityField.text = unicitySlider.value().toInt.toString
    }

    val unicityField = new TextField { editable = false }

    val gridPane = new GridPane {
      padding = Insets(10, 10, 10, 10)
      styleClass += "orch-pane"
      hgap = 5
      vgap = 5
    }

    gridPane.add(nameHBox, 1, 1)
    gridPane.add(stableButton, 1, 2)
    gridPane.add(unstableButton, 2, 2)
    gridPane.add(stabilitySlider, 1, 3)
    gridPane.add(stabilityField, 2, 3)
    gridPane.add(noInvertibilityButton, 1, 4)
    gridPane.add(finiteInvertibilityButton, 2, 4)
    gridPane.add(invertibilitySlider, 1, 5)
    gridPane.add(invertibilityField, 2, 5)
    gridPane.add(noUnicityButton, 1, 6)
    gridPane.add(finiteUnicityButton, 2, 6)
    gridPane.add(unicitySlider, 1, 7)
    gridPane.add(unicityField, 2, 7)

    borderPane.center = gridPane

    def onShow = {
      unstableButton.fire
      stabilitySlider.value = 0
      noInvertibilityButton.fire
      noUnicityButton.fire
      nameField.requestFocus
    }

    def onHide =
      response match {
        case DialogOK => {
          val name : String = nameField.text()

          val stabilityLevel : Option[Int] =
            if (unstableButton.selected()) {
              Some(stabilityField.text().toInt)
            } else None

          val invertibilityLevel : Option[Int] =
            if (finiteInvertibilityButton.selected()) {
              Some(invertibilityField.text().toInt)
            } else None

          val unicityLevel : Option[Int] =
            if (finiteUnicityButton.selected()) {
              Some(unicityField.text().toInt)
            } else None


          val newModule = new JavaFXModule(name, thisEditor, stabilityLevel, invertibilityLevel, unicityLevel) 
          consoleMessage("Created new module named: " ++ name)
          activeModule = newModule
          newModule.newSheet
        }
        case DialogCancel => ()
      }

  }

}
