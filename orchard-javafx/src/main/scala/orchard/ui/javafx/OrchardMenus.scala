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

  implicit def pm : PopupManager

  val openItem = new MenuItem {
    text = "Open"
    onAction = onOpen
  }

  val saveItem = new MenuItem {
    text = "Save"
    onAction = onSave
  }

  val exitItem = new MenuItem {
    text = "Exit"
    onAction = onExit
  }

  val fileMenu = new Menu {
    text = "File"
    items ++= List(openItem, saveItem, exitItem)
  }

  val newDefnItem = new MenuItem {
    text = "New Definition"
    onAction = onNewDefinition
  }

  val completeItem = new MenuItem {
    text = "Complete Using Cell"
    onAction = onCompleteDefinition
  }

  val definitionMenu = new Menu {
    text = "Definition"
    items ++= List(newDefnItem, completeItem)
  }

  val extrudeItem = new MenuItem {
    text = "Extrude Selection"
    onAction = onExtrude
  }

  val dropItem = new MenuItem {
    text = "Extrude Drop"
    onAction = onDrop
  }

  val assumeItem = new MenuItem {
    text = "Assume Variable"
    onAction = onAssume(false)
  }

  val composeItem = new MenuItem {
    text = "Compose Selection"
    onAction = onCompose
  }

  val identityItem = new MenuItem {
    text = "Insert Identity"
    onAction = onInsertIdentity
  }

  val fillItem = new MenuItem {
    text = "Fill Nook"
    onAction = onFill
  }

  val useItem = new MenuItem {
    text = "Use Cell From Environment"
    onAction = onUseEnvironment
  }

  val expressionMenu = new Menu {
    text = "Expression"
    items ++= List(extrudeItem, dropItem, assumeItem, composeItem, identityItem, fillItem, useItem)
  }

  val menuBar = new MenuBar {
    menus ++= List(fileMenu, definitionMenu, expressionMenu)
  }

  def onOpen : Unit
  def onSave : Unit
  def onExit : Unit

  def onNewDefinition : Unit
  def onCompleteDefinition : Unit

  def onExtrude : Unit 
  def onDrop : Unit
  def onAssume(isThin : Boolean) : Unit
  def onCompose : Unit
  def onInsertIdentity : Unit
  def onFill : Unit
  def onUseEnvironment : Unit

}
