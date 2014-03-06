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

trait OrchardMenus { self : JavaFXEditor =>

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

  val newSheetItem = new MenuItem {
    text = "New Sheet"
    onAction = onNewSheet
  }

  val completeItem = new MenuItem {
    text = "Complete Definition"
    onAction = onCompleteDefinition
  }

  val definitionMenu = new Menu {
    text = "Definition"
    items ++= List(newDefnItem, newSheetItem, completeItem)
  }

  val spawnInShellItem = new MenuItem {
    text = "Spawn in Shell"
    onAction = onSpawnInShell
  }

  val satisfyGoalItem = new MenuItem {
    text = "Satisfy Goal"
    onAction = onSatisfyGoal
  }

  val substitutionMenu = new Menu {
    text = "Substitution"
    items ++= List(spawnInShellItem, satisfyGoalItem)
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
    menus ++= List(fileMenu, definitionMenu, substitutionMenu, expressionMenu)
  }

  def onOpen : Unit
  def onSave : Unit
  def onExit : Unit

  def onNewDefinition : Unit
  def onNewSheet : Unit
  def onCompleteDefinition : Unit

  def onSpawnInShell : Unit
  def onSatisfyGoal : Unit

  def onExtrude : Unit 
  def onDrop : Unit
  def onAssume(isThin : Boolean) : Unit
  def onCompose : Unit
  def onInsertIdentity : Unit
  def onFill : Unit
  def onUseEnvironment : Unit

}
