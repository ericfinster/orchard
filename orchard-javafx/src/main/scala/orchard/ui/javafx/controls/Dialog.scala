/**
  * Dialog.scala - A simple popup dialog class
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx.controls

import scalafx.Includes._

import scalafx.scene.layout.HBox
import scalafx.scene.layout.BorderPane

import scalafx.scene.control.Button
import scalafx.scene.control.Label

import scalafx.geometry.Insets

import javafx.scene.{layout => jfxsl}

abstract class Dialog(implicit pm : PopupManager) extends PopupRegion {

  sealed trait DialogResponse
  case object DialogOK extends DialogResponse
  case object DialogCancel extends DialogResponse

  var response : DialogResponse = DialogOK

  protected val borderPane = new BorderPane { padding = Insets(10,10,10,10) }
  protected val heading = new Label { text = "Dialog" }

  getStyleClass.add("orchard-dialog")
  setMaxSize(jfxsl.Region.USE_PREF_SIZE, jfxsl.Region.USE_PREF_SIZE)
  getChildren.add(borderPane)

  override def layoutChildren = { borderPane.resizeRelocate(0, 0, getWidth, getHeight) }

  protected val okBtn =
    new Button("Ok") {
      onAction = onOkAction
      defaultButton = true
    }

  protected val btnBox =
    new HBox {
      spacing = 10
      content = okBtn
    }

  borderPane.top = heading
  borderPane.bottom = btnBox

  def run = pm.showModal(this)

  def onOkAction = { response = DialogOK ; pm.hideModal }

  def onHide : Unit
  def onShow : Unit

}

abstract class CancellableDialog(implicit pm : PopupManager) extends Dialog {

  protected val cancelBtn =
    new Button("Cancel") {
      onAction = onCancelAction
      cancelButton = true
    }

  btnBox.content += cancelBtn

  def onCancelAction = { response = DialogCancel ; pm.hideModal }

}
