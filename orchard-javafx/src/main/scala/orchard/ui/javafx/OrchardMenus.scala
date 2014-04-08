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

  val newWkspItem = new MenuItem {
    text = "New Workspace"
    onAction = onNewWorkspace
  }

  val newSheetItem = new MenuItem {
    text = "New Sheet"
    onAction = onNewSheet
  }

  val substVarItem = new MenuItem {
    text = "Substitute for Variable"
    onAction = onSubstitute
  }

  val workspaceMenu = new Menu {
    text = "Workspace"
    items ++= List(newWkspItem, newSheetItem, substVarItem)
  }

  val newTemplateItem = new MenuItem {
    text = "Create From Workspace"
    onAction = onNewTemplate
  }

  val applyItem = new MenuItem {
    text = "Apply"
    onAction = onApplyTemplate
  }

  val applyInShellItem = new MenuItem {
    text = "Apply in Shell"
    onAction = onApplyTemplateInShell
  }

  val templateMenu = new Menu {
    text = "Template"
    items ++= List(newTemplateItem, applyItem, applyInShellItem)
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
    items ++= List(extrudeItem, dropItem, assumeItem, fillItem, useItem)
  }

  val menuBar = new MenuBar {
    menus ++= List(fileMenu, workspaceMenu, templateMenu, expressionMenu)
  }

  def onOpen : Unit
  def onSave : Unit
  def onExit : Unit

  def onNewWorkspace : Unit
  def onNewSheet : Unit
  def onSubstitute : Unit

  def onNewTemplate : Unit
  def onApplyTemplate : Unit
  def onApplyTemplateInShell : Unit

  def onExtrude : Unit 
  def onDrop : Unit
  def onAssume(isThin : Boolean) : Unit
  def onFill : Unit
  def onUseEnvironment : Unit

}
