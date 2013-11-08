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

    protected val cancelBtn =
      new Button("Cancel") { 
        onAction = onCancelAction
        cancelButton = true 
      }

    borderPane.top = heading
    borderPane.bottom = 
      new HBox {
        spacing = 10
        content = List(okBtn, cancelBtn) 
      }

    def run = push(this)

    def onOkAction = { response = DialogOK ; pop } 
    def onCancelAction = { response = DialogCancel ; pop } 

    def onHide : Unit
    def onShow : Unit

  }

  // class Dialog extends jfxsl.Region {

  //   private val borderPane = new BorderPane { padding = Insets(10,10,10,10) }
  //   private val heading = new Label { text = "Dialog" }

  //   getStyleClass.add("orchard-dialog")
  //   setMaxSize(jfxsl.Region.USE_PREF_SIZE, jfxsl.Region.USE_PREF_SIZE)
  //   getChildren.add(borderPane)

  //   override def layoutChildren = { borderPane.resizeRelocate(0, 0, getWidth, getHeight) }

  //   val okBtn = 
  //     new Button("Ok") { 
  //       onAction = { response = DialogOK ; hide } 
  //     }

  //   val cancelBtn = 
  //     new Button("Cancel") { 
  //       onAction = { response = DialogCancel ; hide } 
  //       cancelButton = true 
  //     }

  //   def title : String = heading.text()
  //   def title_=(str : String) = { heading.text = str }

  //   def body : Node = borderPane.center()
  //   def body_=(n : Node) = { borderPane.center = n }

  //   def show = push(this)
  //   def hide = if (peek == this) pop

  //   borderPane.top = heading
  //   borderPane.bottom = 
  //     new HBox {
  //       spacing = 10
  //       content = List(okBtn, cancelBtn) 
  //     }

  //   private var onShowFun : () => Unit = null
  //   private var onHideFun : DialogResponse => Unit = null

  //   def onShow = { if (onShowFun != null) onShowFun() }
  //   def onShow_=(op : => Unit) = onShowFun = { () => op }

  //   def onHide = { if (onHideFun != null) onHideFun(response) }
  //   def onHide_=(op : DialogResponse => Unit) = onHideFun = op

  //   var response : DialogResponse = DialogOK

  //   def run(cb : DialogResponse => Unit) = {

  //     show
  //   }
  // }
}
