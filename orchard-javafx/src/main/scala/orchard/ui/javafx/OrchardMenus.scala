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

  val exitItem = new MenuItem {
    text = "Exit"
    onAction = onExit
  }

  val fileMenu = new Menu {
    text = "File"
    items ++= List(exitItem)
  }

  val newModuleItem = new MenuItem {
    text = "New Module"
    onAction = onNewModule
  }

  val openModuleItem = new MenuItem {
    text = "Open Module"
    onAction = onOpenModule
  }

  val saveModuleItem = new MenuItem {
    text = "Save Module"
    onAction = onSaveModule
  }

  val createDefinitionItem = new MenuItem {
    text = "Create Definition"
    onAction = onCreateDefinition
  }

  val deleteDefinitionItem = new MenuItem {
    text = "Delete Definition"
    onAction = onDeleteDefinition
  }

  val moduleMenu = new Menu {
    text = "Module"
    items ++= List(newModuleItem, openModuleItem, saveModuleItem, createDefinitionItem, deleteDefinitionItem)
  }

  val newWkspItem = new MenuItem {
    text = "New Workspace"
    onAction = onNewWorkspace
  }

  val closeWkspItem = new MenuItem {
    text = "Close Workspace"
    onAction = onCloseWorkspace
  }

  val newSheetItem = new MenuItem {
    text = "New Sheet"
    onAction = onNewSheet
  }

  val workspaceMenu = new Menu {
    text = "Workspace"
    items ++= List(newWkspItem, closeWkspItem, newSheetItem)
  }

  val newSubstItem = new MenuItem {
    text = "New Substitution"
    onAction = onNewSubstitution
  }

  val newSubstInShellItem = new MenuItem {
    text = "New Substitution in Shell"
    onAction = onNewSubstInShell
  }

  val importItem = new MenuItem {
    text = "Import to Environment"
    onAction = onImportSubstitution
  }

  val cancelItem = new MenuItem {
    text = "Cancel Substitution"
    onAction = onCancelSubstitution
  }

  val substitutionMenu = new Menu {
    text = "Substitution"
    items ++= List(newSubstItem, newSubstInShellItem, importItem, cancelItem)
  }

  val assumeItem = new MenuItem {
    text = "Assume Variable"
    onAction = onAssume(false)
  }

  val fillItem = new MenuItem {
    text = "Fill Exposed Nook"
    onAction = onFill
  }

  val pasteItem = new MenuItem {
    text = "Paste from Environment"
    onAction = onPaste
  }

  val bindVarItem = new MenuItem {
    text = "Bind Variable"
    onAction = onBind
  }

  val abstractItem = new MenuItem {
    text = "Abstract Expression"
    onAction = onAbstract
  }

  val expressionMenu = new Menu {
    text = "Expression"
    items ++= List(assumeItem, fillItem, pasteItem, bindVarItem, abstractItem)
  }

  val extrudeItem = new MenuItem {
    text = "Extrude Selection"
    onAction = onExtrude
  }

  val dropItem = new MenuItem {
    text = "Extrude Drop"
    onAction = onDrop
  }

  val shapeMenu = new Menu {
    text = "Shape"
    items ++= List(extrudeItem, dropItem)
  }

  val menuBar = new MenuBar {
    menus ++= List(fileMenu, moduleMenu, workspaceMenu, substitutionMenu, expressionMenu, shapeMenu)
  }

  def onExit : Unit

  def onNewModule : Unit
  def onOpenModule : Unit
  def onSaveModule : Unit
  def onCreateDefinition : Unit
  def onDeleteDefinition : Unit

  def onNewWorkspace : Unit
  def onCloseWorkspace : Unit
  def onNewSheet : Unit

  def onNewSubstitution : Unit
  def onNewSubstInShell : Unit
  def onImportSubstitution : Unit
  def onCancelSubstitution : Unit

  def onAssume(isThin : Boolean) : Unit
  def onFill : Unit
  def onPaste : Unit
  def onAbstract : Unit
  def onBind : Unit

  def onExtrude : Unit 
  def onDrop : Unit


}
