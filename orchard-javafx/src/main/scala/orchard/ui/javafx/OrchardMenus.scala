/**
  * OrchardMenus.scala - Mixin trait with our menu definitions
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import orchard.ui.javafx.controls.Menu
import orchard.ui.javafx.controls.MenuBar
import orchard.ui.javafx.controls.MenuItem

import orchard.ui.javafx.controls.PopupManager

trait OrchardMenus {

  implicit val pm : PopupManager

  val openItem = new MenuItem {
    text = "Open"
    onAction = onOpen
  }

  def onOpen : Unit

  val saveItem = new MenuItem {
    text = "Save"
    onAction = onSave
  }

  def onSave : Unit

  val exitItem = new MenuItem {
    text = "Exit"
    onAction = onExit
  }

  def onExit : Unit

  val fileMenu = new Menu {
    text = "File"
    items ++= List(openItem, saveItem, exitItem)
  }

  val fromCell = new MenuItem {
    text = "New From Cell"
  }

  val fromEnvironment = new MenuItem {
    text = "New From Environment"
    onAction = onDefnFromEnv
  }

  def onDefnFromEnv : Unit

  val definitionMenu = new Menu {
    text = "Definition"
    items ++= List(fromCell, fromEnvironment)
  }

  val extrudeItem = new MenuItem {
    text = "Extrude Selection"
    onAction = onExtrude
  }

  def onExtrude : Unit 

  val dropItem = new MenuItem {
    text = "Extrude Drop"
    onAction = onDrop
  }

  def onDrop : Unit

  val assumeItem = new MenuItem {
    text = "Assume Variable"
    onAction = onAssume(false)
  }

  def onAssume(isThin : Boolean) : Unit

  val composeItem = new MenuItem {
    text = "Compose Selection"
    onAction = onCompose
  }

  def onCompose : Unit

  val identityItem = new MenuItem {
    text = "Insert Identity"
    onAction = onInsertIdentity
  }

  def onInsertIdentity : Unit

  val fillItem = new MenuItem {
    text = "Fill Nook"
    onAction = onFill
  }

  def onFill : Unit

  val useItem = new MenuItem {
    text = "Use Cell From Environment"
    onAction = onUseEnvironment
  }

  def onUseEnvironment : Unit

  val expressionMenu = new Menu {
    text = "Expression"
    items ++= List(extrudeItem, dropItem, assumeItem, composeItem, identityItem, fillItem, useItem)
  }

  val menuBar = new MenuBar {
    menus ++= List(fileMenu, definitionMenu, expressionMenu)
  }


}
