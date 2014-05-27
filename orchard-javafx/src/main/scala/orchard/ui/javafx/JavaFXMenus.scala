/**
  * JavaFXMenus.scala - Menu implementations
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import controls._

trait JavaFXMenus { thisEditor : JavaFXEditor =>

  val exitItem = new MenuItem {
    text = "Exit"
    onAction = onExit
  }

  val fileMenu = new Menu {
    text = "File"
    items ++= List(exitItem)
  }

  val menuBar = new MenuBar {
    menus ++= List(fileMenu)
  }

}
