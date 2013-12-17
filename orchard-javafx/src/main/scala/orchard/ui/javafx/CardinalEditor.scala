/**
  * CardinalEditor.scala - A simple cardianl editor
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scala.collection.mutable.Set
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap

import scalafx.Includes._

import scalafx.scene.control.Label
import scalafx.scene.control.Button
import scalafx.scene.control.Accordion
import scalafx.scene.control.TitledPane
import scalafx.scene.control.TextField

import scalafx.scene.layout.HBox
import scalafx.scene.layout.VBox
import scalafx.scene.layout.Region
import scalafx.scene.layout.GridPane
import scalafx.scene.layout.StackPane
import scalafx.scene.layout.BorderPane

import scalafx.geometry.Insets
import scalafx.geometry.Orientation

import scalafx.stage.FileChooser

import javafx.scene.{layout => jfxsl}

import orchard.core._
import Util._
import Nats._
import XmlSerializable._

class CardinalEditor extends DialogStack(new BorderPane) with EventReactor[CellEvent] { editor =>

  //============================================================================================
  // UI INITIALIZATION
  //

  getStyleClass.add("cardinal-editor")

  val fileChooser = new FileChooser
  val borderPane = new BorderPane(root.asInstanceOf[jfxsl.BorderPane])

  setGallery(new CardinalGallery(CardinalComplex(Object("a"))))

  borderPane.left = 
    new StackPane {
      padding = Insets(10, 0, 10, 10)
      style = "-fx-background-color: gainsboro"
      content = new Accordion {
        panes = List(
          new TitledPane {
            text = "File"
            content = new VBox {
              padding = Insets(10,10,10,10)
              content = List(
                new Button("New") { prefWidth = 100 ; onAction = NewDialog.run },
                new Button("Open") { prefWidth = 100 ; onAction = onOpenAction },
                new Button("Save") { prefWidth = 100 ; onAction = onSaveAction })
            }
          },
          new TitledPane {
            text = "Action"
            content = new VBox {
              padding = Insets(10,10,10,10)
              content = List(
                new Button("Compose") { prefWidth = 100 ; onAction = ComposeDialog.run },
                new Button("Extend") { prefWidth = 100 ; onAction =  { if (gallery != null) gallery.complex.extend } },
                new Button("Drop") { prefWidth = 100 ; onAction = DropDialog.run },
                new Button("Dump") { prefWidth = 100 ; onAction = gallery.complex.forAllCells(c => println(c.toString)) })
            }
          })
      }
    }

  borderPane.bottom = 
    new StackPane {
      padding = Insets(0,0,10,0)
      style = "-fx-background-color: gainsboro"
      content =
        new HBox {
          spacing = 10
          maxWidth = Region.USE_PREF_SIZE
          maxHeight = Region.USE_PREF_SIZE
          content = List(
            new Button("Prev") { onAction = { if (gallery != null) gallery.prev } },
            new Button("Next") { onAction = { if (gallery != null) gallery.next } })
        }
    }

  //============================================================================================
  // DIALOG DEFINITIONS
  //

  object NewDialog extends Dialog {

    val label = new Label("Initial object:")
    val field = new TextField { onAction = okBtn.fire }

    heading.text = "New"
    borderPane.center = new HBox {
      content = List(label, field)
      spacing = 10
      padding = Insets(10,10,10,10)
    }

    def onShow = { 
      field.clear 
      field.requestFocus 
    }

    def onHide = 
      response match {
        case DialogOK => newGallery(field.text())
        case DialogCancel => ()
      }
  }

  abstract class TwoCellDialog extends Dialog {
    val univField = new TextField { onAction = { okBtn.fire } }
    val compField = new TextField { onAction = { univField.requestFocus } }

    val grid =
      new GridPane {
        hgap = 5
        vgap = 5
        padding = Insets(10,10,10,10)
      }

    grid.addRow(0, new Label("Composite:"), compField)
    grid.addRow(1, new Label("Universal:"), univField)

    heading.text = "Compose"
    borderPane.center = grid

    def onShow = { 
      univField.clear 
      compField.clear
      compField.requestFocus 
    }
  }

  object ComposeDialog extends TwoCellDialog {
    def onHide =
      response match {
        case DialogOK => composeSelection(compField.text(), univField.text())
        case DialogCancel => ()
      }
  }

  object DropDialog extends TwoCellDialog {
    def onHide =
      response match {
        case DialogOK => insertDrop(compField.text(), univField.text())
        case DialogCancel => ()
      }
  }

  //============================================================================================
  // SELECTION
  //

  type EditorCell = CardinalPanel[String]#CardinalCell

  var selectionBase : Option[EditorCell] = None
  val selectedCells : Set[EditorCell] = new HashSet

  def selectionIsComposable : Boolean = 
    selectionBase match {
      case None => false
      case Some(base) => {
        val baseContainer = base.container.force("Selection has no container.")
        if (baseContainer.owner.isPositive) true else false
      }
    }

  def deselectAll = {
    selectedCells foreach
    (cell => cell.owner.emitToFaces(RequestDeselected))
    selectedCells.clear
    selectionBase = None
  }

  def clearAndSelect(cell : EditorCell) = {
    deselectAll
    selectAsBase(cell)
  }

  def selectAsBase(cell : EditorCell) = {
    select(cell)
    selectionBase = Some(cell)
  }

  def isSelected(cell : EditorCell) =
    selectedCells contains cell

  def trySelect(cell : EditorCell) : Boolean = {
    // This should be guaranteed by the cardinal structure
    val base = selectionBase.force
    val baseContainer = base.container.force

    if (cell.container.force != baseContainer)
      return false

    val candidates : Set[EditorCell] = new HashSet
    candidates add cell

    // Now look up a zipper to this guy
    val zipper : RoseZipper[EditorCell, Int] = 
      new RoseZipper(baseContainer.canopy.force, Nil)
    var ptrOpt = zipper.lookup(cell)
      .force("Lookup failed for selected cell.").zipOnce

    // Step back through the zipper and look for the base selection
    while (ptrOpt != None) {
      val ptr = ptrOpt.force
      val testCell = ptr.focus.rootElement.force("No root element?")

      if (isSelected(testCell)) {
        // We're done!!
        candidates foreach (c => select(c))
        return true
      } else {
        candidates add testCell
        ptrOpt = ptr.zipOnce
      }
    }

    return false
  }

  def select(cell : EditorCell) = {
    cell.owner.emitToFaces(RequestSelected)
    selectedCells add cell
  }

  //============================================================================================
  // EVENTS
  //

  def onEventEmitted(ev : CellEvent) = 
    ev match {
      case CellClicked(c) => {
        val cell = c.asInstanceOf[EditorCell]

        if (cell.owner.isNeutral)
          clearAndSelect(cell)
        else
          deselectAll
      }

      case CellCtrlClicked(c) => {
        val cell = c.asInstanceOf[EditorCell]

        selectionBase match {
          case None => if (cell.owner.isNeutral) selectAsBase(cell)
          case Some(base) => {
            if (cell != base) {
              if (cell.owner.isPolarized) {
                deselectAll
              } else {
                if (!trySelect(cell)) clearAndSelect(cell)
              }
            }
          }
        }
      }

      case CellDoubleClicked(c) => ()

      case _ => ()
    }

  def onSaveAction = {
    fileChooser.setTitle("Save")

    val file = fileChooser.showSaveDialog(getScene.getWindow)
    val galleryXml = cellSerializable[Polarity[String]].toXML(gallery.complex.toCell)

    if (file != null) {
      xml.XML.save(file.getAbsolutePath, galleryXml)
    }
  }

  def onOpenAction = {
    fileChooser.setTitle("Open")

    val file = fileChooser.showOpenDialog(getScene.getWindow)

    if (file != null) {
      val elem = xml.XML.loadFile(file.getAbsolutePath)
      val result = cellSerializable[Polarity[String]].fromXML(elem)

      setGallery(new CardinalGallery[String](result))
    }
  }

  //============================================================================================
  // SEMANTICS
  //

  def gallery : CardinalGallery[String] = 
    borderPane.center().asInstanceOf[CardinalGallery[String]]

  def setGallery(g : CardinalGallery[String]) = {
    g.renderAll
    reactTo(g)

    borderPane.center = g
  }

  def newGallery(initialObj : String) = {
    // Uhhh .. should we clean up the old one somehow???
    setGallery(new CardinalGallery[String](CardinalComplex(Object(initialObj))))
  }

  def composeSelection(composite : String, universal : String) =
    selectionBase match {
      case None => ()
      case Some(base) => {
        if (base.owner.dimension >= gallery.dimension - 1)
          gallery.complex.extend

        val currentComplex = gallery.complex

        // I think the casting is unnecessary: you know that you're looking
        // for the positive container, which you can get by hand ... see below
        val baseContainer = 
          base.owner.container.force.asInstanceOf[currentComplex.CellType]

        val basePtr = (new RoseZipper(baseContainer.canopy.force, Nil))
          .lookup(base.owner.asInstanceOf[currentComplex.CellType])
          .force("Lookup failed for selection base")

        val owners = selectedCells map (_.owner.asInstanceOf[currentComplex.CellType])

        baseContainer.insertComposite(Neutral(composite), Neutral(universal), basePtr, (cell => owners contains cell))
      }
    }

  def insertDrop(loopStr : String, dropStr : String) = 
    selectionBase match {
      case None => ()
      case Some(base) => {
        if (base.owner.dimension == gallery.dimension - 2) {
          gallery.complex.extend
        } else if (base.owner.dimension == gallery.dimension - 1) {
          gallery.complex.extend ; gallery.complex.extend
        }

        val activePanel = gallery(base.owner.dimension + 1)
        val positiveBase = activePanel.baseCell.owner
        val zipper = new RoseZipper(positiveBase.canopy.force, Nil)

        val basePtr = positiveBase.canopy.force match {
          case Rose(_) => throw new IllegalArgumentException("Negative cell has no sources.")
          case Branch(negCell, brs) => {
            val i = brs indexWhere
              (branch =>
                branch match {
                  case Rose(idx) => {
                    val srcs = positiveBase.sources.force
                    if (srcs(idx) == base.owner) true else false
                  }
                  case Branch(cell, _) => {
                    if (cell.target.force == base.owner) true else false
                  }
                })

            zipper.visitBranch(i).force
          }
        }

        positiveBase.insertComposite(Neutral(loopStr), Neutral(dropStr), basePtr, (_ => false))
      }
    }
}
