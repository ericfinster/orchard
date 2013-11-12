/**
  * DialogStack.scala - A node which can spawn modal dialogs
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.Includes._

import scalafx.scene.Node
import scalafx.scene.layout.HBox
import scalafx.scene.layout.StackPane
import scalafx.scene.layout.BorderPane

import scalafx.scene.effect.ColorAdjust

import scalafx.scene.control.Label
import scalafx.scene.control.Button

import scalafx.animation.Timeline
import scalafx.animation.KeyValue
import scalafx.animation.KeyFrame

import scalafx.geometry.Insets

import javafx.scene.{layout => jfxsl}

class DialogStack(initialRoot : Node) extends jfxsl.Region {

  val stack = new StackPane {
    content = initialRoot
  }

  getChildren.add(stack)

  override def layoutChildren = { stack.resizeRelocate(0, 0, getWidth, getHeight) }

  def root = stack.content(0)
  def root_=(r : Node) = stack.content(0) = r

  def peek = stack.content.last

  def push(n : Dialog) = {
    peek.effect = new ColorAdjust { brightness = -0.5 }
    peek.disable = true
    stack.content += n

    // TODO: Animate the effect

    n.onShow
  }

  def pop = {
    val current = peek.asInstanceOf[Dialog]

    stack.content -= current
    peek.disable = false
    peek.effect = null

    // TODO: Animate the effect

    current.onHide
  }

  abstract class Dialog extends jfxsl.Region {

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

    def run = push(this)

    def onOkAction = { response = DialogOK ; pop } 

    def onHide : Unit
    def onShow : Unit

  }

  abstract class CancellableDialog extends Dialog {

    protected val cancelBtn =
      new Button("Cancel") { 
        onAction = onCancelAction
        cancelButton = true 
      }

    btnBox.content += cancelBtn

    def onCancelAction = { response = DialogCancel ; pop } 

  }

}
