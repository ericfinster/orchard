/**
  * JavaFXWorkspace.scala - JavaFX implementation of a workspace
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.Includes._

import scalafx.geometry._

import scalafx.scene.layout._
import scalafx.scene.control._

import orchard.core.ui._
import orchard.core.cell._
import orchard.core.editor._
import orchard.core.complex._
import orchard.core.expression._

import javafx.scene.{control => jfxsc}

class JavaFXWorkspace(
  val editor : JavaFXEditor,
  val name : String,
  val stabilityLevel : Option[Int],
  val invertibilityLevel : Option[Int],
  val unicityLevel : Option[Int]
) extends Workspace with JavaFXWorksheetEnv with JavaFXFrameworkEnv {

  var activeExpression : Option[NCell[Expression]] = None
  var activeGallery : Option[WorksheetGallery] = None 

  var sheetCount : Int = 1

  def activeSheet : Option[Worksheet] =
    for {
      gallery <- activeGallery
    } yield gallery.complex

  val sheetTabPane = new TabPane {
    side = Side.TOP
  }

  def newSheet = newSheet(CardinalComplex(Object(None)))

  def newSheet(seed : NCell[Polarity[Option[Expression]]]) : Unit = {
    val gallery = new WorksheetGallery(seed)

    val tab = new Tab {
      text = "Sheet " ++ sheetCount.toString
      content = gallery

      onClosed = () => {
        sheets -= gallery.complex
      }

      onSelectionChanged = () => {
        if (selected())
          activeGallery = Some(gallery)
      }
    }

    sheetTabPane += tab
    sheetTabPane.getSelectionModel.select(tab)
    sheets += gallery.complex
    sheetCount += 1
    gallery.refreshAll

  }

  val environmentRoot = new TreeItem[EnvironmentNode]
  val environmentView =
    new TreeView[EnvironmentNode] {
      root = environmentRoot
      showRoot = false
      cellFactory = (_ => new EnvironmentTreeCell)
    }

  override def addToEnvironment(expr : NCell[Expression]) = {
    val node = super.addToEnvironment(expr)

    environmentRoot.children add
      new TreeItem[EnvironmentNode] {
        value = node
      }.delegate

    node
  }

  def buildEnvironmentTreeItems(node : EnvironmentNode) : TreeItem[EnvironmentNode] = 
    node match {
      case g @ GroupNode(name) => {
        val item = 
          new TreeItem[EnvironmentNode] {
            value = g
          }

        item.children ++= g.children map (buildEnvironmentTreeItems(_).delegate)
        item
      }
      case e @ ExpressionNode(expr) => {
        new TreeItem[EnvironmentNode] {
          value = e
        }
      }
    }


  override def addToEnvironment(node : EnvironmentNode) = {
    super.addToEnvironment(node)

    node match {
      case g @ GroupNode(name) => {
        environmentRoot.children add buildEnvironmentTreeItems(g).delegate
      }
      case e @ ExpressionNode(expr) => addToEnvironment(expr)
    }
  }

  environmentView.getSelectionModel.selectedItem onChange {
    val item = environmentView.getSelectionModel.selectedItem()

    if (item != null) {
      item.value() match {
        case ExpressionNode(expr) => {
          activeExpression = Some(expr)

          // Now make a gallery and show it in the preview pane
          val gallery = new FrameworkGallery(expr map (Some(_)))
          editor.setPreviewGallery(gallery)
        }
        case _ => activeExpression = None
      }
    } else {
      activeExpression = None
    }
  }

  class EnvironmentTreeCell extends jfxsc.TreeCell[EnvironmentNode] {

    getStyleClass add "orch-list-cell"
    val cellStyleIndex = getStyleClass.length
    getStyleClass add "orch-list-null"

    def setCellStyleType(str : String) = {
      getStyleClass(cellStyleIndex) = str
    }

    override def updateItem(node : EnvironmentNode, empty : Boolean) = {
      super.updateItem(node, empty)

      if (! empty) {
        node match {
          case GroupNode(name) => {
            setCellStyleType("orch-list-null")
            setText(name)
          }
          case ExpressionNode(expr) => {
            setCellStyleType("orch-list-cell-" ++ expr.value.styleString)
            setText(expr.value.ident.toString)
          }
        }
      } else {
        setCellStyleType("orch-list-null")
        setText("")
      }
    } 
  }

}
