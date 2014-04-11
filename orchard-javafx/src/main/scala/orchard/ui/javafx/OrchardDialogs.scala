/**
  * OrchardDialogs.scala - Mixin Trait with dialog definitions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.Includes._

import scalafx.geometry._

import scalafx.scene.layout._
import scalafx.scene.control._

import orchard.ui.javafx.controls.Dialog
import orchard.ui.javafx.controls.PopupManager
import orchard.ui.javafx.controls.CancellableDialog

import orchard.core.cell._
import orchard.core.expression._

trait OrchardDialogs { self : JavaFXEditor =>

  //============================================================================================
  // DIALOG DEFINITIONS
  //

  trait DependencyDialog extends CancellableDialog {

    val dependenciesList = new ListView[String] {
      // cellFactory = (_ => new EnvironmentCell )
    }

  }

  class NewModuleDialog extends CancellableDialog {

    heading.text = "New Module"

    val nameField = new TextField { promptText = "Module Name" ; onAction = () => { okBtn.fire } }

    borderPane.center = 
      new StackPane {
        padding = Insets(10, 10, 10, 10)
        content = nameField
      }

    def onShow = {
      nameField.clear
      nameField.requestFocus
    }

    def onHide = {
      response match {
        case DialogOK => newModule(nameField.text())
        case DialogCancel => ()
      }
    }

  }

  class VariableDialog(handler : (String, Boolean) => Unit) extends DependencyDialog {

    heading.text = "Assume Variable"

    val idField = new TextField { promptText = "Identifier" ; onAction = () => { okBtn.fire } }
    val thinCheckBox = new CheckBox("Thin") { allowIndeterminate = false }

    borderPane.center =
      new VBox {
        padding = Insets(10,10,10,10)
        spacing = 10
        content = List(dependenciesList, idField, thinCheckBox)
      }

    def onShow = {
      idField.clear
      idField.requestFocus
    }

    def onHide =
      response match {
        case DialogOK => handler(idField.text(), thinCheckBox.selected())
        case DialogCancel => ()
      }

  }

  class FillingDialog(handler : (String, String) => Unit) extends DependencyDialog {

    val composeField = new TextField { promptText = "Composite" ; onAction = () => { fillerField.requestFocus } }
    val fillerField = new TextField { promptText = "Filler" ; onAction = () => { okBtn.fire } }

    composeField.text onChange {
      fillerField.text = "def-" ++ composeField.text()
    }

    borderPane.center =
      new VBox {
        padding = Insets(10,10,10,10)
        spacing = 10
        content = List(dependenciesList, composeField, fillerField)
      }

    def onShow = {
      composeField.clear
      fillerField.clear
      composeField.requestFocus
    }

    def onHide =
      response match {
        case DialogOK => handler(composeField.text(), fillerField.text())
        case DialogCancel => ()
      }

  }

  class UniqueFillingDialog(handler : String => Unit) extends DependencyDialog {

    val fillerField = new TextField { promptText = "Filler" ; onAction = () => { okBtn.fire } }

    borderPane.center =
      new VBox {
        padding = Insets(10,10,10,10)
        spacing = 10
        content = List(dependenciesList, fillerField)
      }

    def onShow = {
      fillerField.clear
      fillerField.requestFocus
    }

    def onHide =
      response match {
        case DialogOK => handler(fillerField.text())
        case DialogCancel => ()
      }

  }

  class SubstitutionDialog(varExpr : NCell[Expression], envView : TreeView[EnvironmentNode]) extends CancellableDialog {

    heading.text = "Substitution"

    val varGallery = new FrameworkGallery(varExpr map (Some(_)))

    val varPane = new StackPane {
      content = varGallery
      prefWidth = 500
      prefHeight = 200
    }

    val envPane = new StackPane {
      maxWidth = 500
      maxHeight = 200
      content = envView
      padding = Insets(10,10,10,10)
    }

    val selectionPane = new StackPane {
      prefWidth = 500
      prefHeight = 200
    }

    borderPane.center =
      new VBox {
        content = List(varPane, envPane, selectionPane)
      }

    envView.getSelectionModel.selectedItem onChange {
      val item = envView.getSelectionModel.selectedItem()

      if (item != null) {
        item.value() match {
          case ExpressionNode(expr) => {
            val gallery = new FrameworkGallery(expr map (Some(_)))
            selectionPane.content.clear
            selectionPane.content += gallery
            gallery.refreshAll
          }
          case _ => ()
        }
      }
    }

    def onShow = {
      varGallery.refreshAll
    }

    def onHide =
      response match {
        case DialogOK => {
          for { wksp <- activeWorkspace } { 
            val item = envView.getSelectionModel.selectedItem()

            if (item != null) {
              item.value() match {
                case ExpressionNode(expr) => wksp.substitute(varExpr, expr)
                case _ => println("Cannot substitute this ...")
              }
            } else {
              println("Nothing selected.")
            }
          }
        }
        case DialogCancel => ()
      }

  }

  object NewWorkspaceDialog extends CancellableDialog {

    heading.text = "New Workspace"

    val nameLabel = new Label("Name: ")
    val nameField = new TextField { promptText = "Workspace name" }
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


          newWorkspace(name, stabilityLevel, invertibilityLevel, unicityLevel)
        }
        case DialogCancel => ()
      }

  }


}
