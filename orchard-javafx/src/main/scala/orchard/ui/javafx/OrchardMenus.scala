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

  val exitItem = new MenuItem {
    text = "Exit"
    onAction = onExit
  }

  val fileMenu = new Menu {
    text = "File"
    items ++= List(newModuleItem, openModuleItem, saveModuleItem, exitItem)
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

  val exportTemplateItem = new MenuItem {
    text = "Export To Module"
    onAction = onExportTemplate
  }

  val importItem = new MenuItem {
    text = "Import Template"
    onAction = onImportTemplate
  }

  val importInShellItem = new MenuItem {
    text = "Import Template in Shell"
    onAction = onImportTemplateInShell
  }

  val workspaceMenu = new Menu {
    text = "Workspace"
    items ++= List(newWkspItem, closeWkspItem, newSheetItem, exportTemplateItem, importItem, importInShellItem)
  }

  val assumeItem = new MenuItem {
    text = "Assume Variable"
    onAction = onAssume(false)
  }

  val fillItem = new MenuItem {
    text = "Fill Exposed Nook"
    onAction = onFill
  }

  val useItem = new MenuItem {
    text = "Use Environment Cell"
    onAction = onUseEnvironment
  }

  val substVarItem = new MenuItem {
    text = "Substitute for Variable"
    onAction = onSubstitute
  }

  val abstractItem = new MenuItem {
    text = "Abstract Expression"
    onAction = onAbstract
  }

  val expressionMenu = new Menu {
    text = "Expression"
    items ++= List(assumeItem, fillItem, useItem, substVarItem, abstractItem)
  }

  val menuBar = new MenuBar {
    menus ++= List(fileMenu, workspaceMenu, expressionMenu, shapeMenu)
  }

  def onNewModule : Unit
  def onOpenModule : Unit
  def onSaveModule : Unit
  def onExit : Unit

  def onNewWorkspace : Unit
  def onCloseWorkspace : Unit
  def onNewSheet : Unit
  def onExportTemplate : Unit
  def onImportTemplate : Unit
  def onImportTemplateInShell : Unit

  def onAssume(isThin : Boolean) : Unit
  def onFill : Unit
  def onUseEnvironment : Unit
  def onSubstitute : Unit
  def onAbstract : Unit

  def onExtrude : Unit 
  def onDrop : Unit


}
