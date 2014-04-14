/**
  * JavaFXEnvironment.scala - Implementation of a cell environment for JavaFX
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import orchard.core.expression._

import scalafx.Includes._
import scalafx.collections._

import javafx.scene.control.TreeItem
import javafx.scene.control.TreeCell

object JavaFXEnvironment extends EnvironmentType[TreeItem[EnvironmentElement]] {

  type ChildCollectionType = ObservableBuffer[TreeItem[EnvironmentElement]]

  def children(item : TreeItem[EnvironmentElement]) = item.children

  def getElement(item : TreeItem[EnvironmentElement]) = item.value()
  def setElement(item : TreeItem[EnvironmentElement], el : EnvironmentElement) = item.value = el

  def getParent(item : TreeItem[EnvironmentElement]) = {
    if (item.parent() == null) None else Some(item.parent())
  }
  def setParent(item : TreeItem[EnvironmentElement], parent : TreeItem[EnvironmentElement]) = ()

  def createNode : TreeItem[EnvironmentElement] = new TreeItem[EnvironmentElement]
  def createNode(el : EnvironmentElement) = {
    val node = new TreeItem[EnvironmentElement]
    node.setValue(el)
    node
  }

  class EnvironmentTreeCell extends TreeCell[EnvironmentElement] {

    getStyleClass add "orch-list-cell"
    val cellStyleIndex = getStyleClass.length
    getStyleClass add "orch-list-null"

    def setCellStyleType(str : String) = {
      getStyleClass(cellStyleIndex) = str
    }

    override def updateItem(el : EnvironmentElement, empty : Boolean) = {
      super.updateItem(el, empty)

      if (! empty) {
        el match {
          case GroupElement(name) => {
            setCellStyleType("orch-list-null")
            setText(name)
          }
          case ExpressionElement(ncell) => {
            setCellStyleType("orch-list-cell-" ++ ncell.value.styleString)
            setText(ncell.value.id)
          }
        }
      } else {
        setCellStyleType("orch-list-null")
        setText("")
      }
    } 
  }

}
