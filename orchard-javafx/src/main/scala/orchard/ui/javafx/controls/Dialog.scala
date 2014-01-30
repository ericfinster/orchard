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
import scalafx.scene.layout.AnchorPane

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

  override def layoutChildren = { 
    super.layoutChildren
    borderPane.relocate(getInsets.getLeft, getInsets.getTop)
  }

  protected val okBtn =
    new Button("Ok") {
      onAction = () => { onOkAction }
      defaultButton = true
      styleClass += "orch-btn"
    }

  protected val btnBox =
    new HBox {
      spacing = 10
      content = okBtn
    }

  protected val anchorPane = new AnchorPane {
    content = btnBox
  }

  AnchorPane.setTopAnchor(btnBox, 0)
  AnchorPane.setRightAnchor(btnBox, 0)
  AnchorPane.setBottomAnchor(btnBox, 0)

  borderPane.top = heading
  borderPane.bottom = anchorPane

  def run = pm.showModal(this)

  def onOkAction = { response = DialogOK ; pm.hideModal }

  def onHide : Unit
  def onShow : Unit

}

abstract class CancellableDialog(implicit pm : PopupManager) extends Dialog {

  protected val cancelBtn =
    new Button("Cancel") {
      onAction = () => { onCancelAction }
      cancelButton = true
      styleClass += "orch-btn"
    }

  btnBox.content += cancelBtn

  def onCancelAction = { response = DialogCancel ; pm.hideModal }

}
