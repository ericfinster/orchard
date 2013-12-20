/**
  * PopupManager.scala - A base region for managing regions which pop up
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx.controls

import scala.collection.JavaConversions._
import scala.collection.mutable.HashMap

import scalafx.Includes._

import scalafx.scene.Node

import scalafx.scene.effect.ColorAdjust

import javafx.scene.{layout => jfxsl}

abstract class PopupRegion extends jfxsl.Region

class PopupManager(r : Node) extends jfxsl.Region {

  val positionMap = new HashMap[PopupRegion, (Double, Double)]

  getChildren add r

  def root : Node = {
    val myChildren = getChildren
    myChildren(0)
  }

  def root_=(r : Node) = {
    val myChildren = getChildren
    myChildren(0) = r
  }

  def center(p : PopupRegion) = {
    val posX = (getWidth / 2) - (p.getWidth / 2)
    val posY = (getHeight / 2) - (p.getHeight / 2)

    p.relocate(posX, posY)
  }

  def showAt(p : PopupRegion, x : Double, y : Double) = {
    positionMap(p) = (x, y)
    getChildren add p
  }

  def hide(p : PopupRegion) = getChildren remove p

  var currentModal : Option[Dialog] = None

  def showModal(md : Dialog) = 
    currentModal match {
      case None => {
        getChildren foreach (child => {
          child.effect = new ColorAdjust { brightness = -0.5 }
          child.disable = true
        })

        getChildren add md
        currentModal = Some(md)
        md.onShow
      }
      case _ => () // Quietly refuse to do anything
    }

  def hideModal = 
    currentModal match {
      case None => ()
      case Some(md) => {
        md.onHide
        getChildren remove md
        getChildren foreach (child => { child.effect = null ; child.disable = false })
        currentModal = None
      }
    }

  override def layoutChildren = {
    val insets = getInsets
    val left = insets.getLeft
    val right = insets.getRight
    val top = insets.getTop
    val bottom = insets.getBottom

    // Scale the root to take up all the space
    root.resizeRelocate(left, top, getWidth - left - right, getHeight - top - bottom)

    positionMap foreach (pp => {
      val (popup, (xPos, yPos)) = pp
      popup.autosize
      popup.relocate(xPos, yPos)
    })

    currentModal foreach (md => { md.autosize ; center(md) })
  }
}
