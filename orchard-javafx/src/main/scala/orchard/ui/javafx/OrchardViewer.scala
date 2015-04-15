/**
  * OrchardViewer.scala - A Viewer for manipulating/exporting images
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.Includes._

import scalafx.geometry._
import scalafx.scene.layout._
import scalafx.scene.control._
import scalafx.scene.shape._
import scalafx.scene.paint._

import javafx.event.Event
import javafx.event.EventHandler
import javafx.scene.input.KeyCode
import javafx.scene.input.KeyEvent

import javax.imageio.ImageIO
import javafx.scene.SnapshotParameters
import javafx.embed.swing.SwingFXUtils

import javafx.scene.{layout => jfxsl}

import orchard.core.cell._
import orchard.core.expression._

import controls._

class OrchardViewer(implicit pm : PopupManager) extends Dialog { 

  private var activeExpression : Option[NCell[Option[Expression]]] = None
  private var activeGallery : Option[JavaFXGallery[Option[Expression]]] = None

  def panelCount : Int = 
    panelsField.text().toInt

  def setActiveExpression(ncell : NCell[Option[Expression]]) : Unit = {
    activeExpression = Some(ncell)
    renderActiveExpression
  }

  private def renderActiveExpression : Unit = 
    for {
      expr <- activeExpression
    } {
      if (completeViewButton.selected()) {
        val gallery = new StaticFrameworkGallery(expr)
        viewerArea.content = gallery
        activeGallery = Some(gallery)
        gallery.renderAll
      } else {
        val gallery = new FrameworkGallery(expr)
        gallery.length = panelCount
        viewerArea.content = gallery
        activeGallery = Some(gallery)
        gallery.renderAll
      }
    }

  //============================================================================================
  // UI
  //

  heading.text = "Expression Viewer"

  val viewerArea = new StackPane { 
    padding = Insets(10,10,10,10) 
    prefWidth = 1000
    prefHeight = 600
  }

  val completeViewButton = new RadioButton("Complete") {
    selected = true
    onAction = { () => 
      panelsLabel.disable = true
      panelDecButton.disable = true
      panelIncButton.disable = true
      panelsField.disable = true
      panelLeftButton.disable = true
      panelRightButton.disable = true
      renderActiveExpression
    }
  }

  val paneledViewButton = new RadioButton("Paneled") {
    onAction = { () => 
      panelsLabel.disable = false
      panelDecButton.disable = false
      panelIncButton.disable = false
      panelsField.disable = false
      panelLeftButton.disable = false
      panelRightButton.disable = false
      renderActiveExpression
    }
  }

  val viewToggle = new ToggleGroup {
    toggles = List(completeViewButton, paneledViewButton)
  }

  val panelsLabel = new Label("Panels") {
    disable = true
  }

  val panelDecButton = new Button {
    graphic = new Polygon {
      points ++= List(-5.0, 0.0, 0.0, 5.0, 5.0, 0.0)
      fill = Color.BLACK
      stroke = Color.BLACK
    }
    onAction = { () => 
      if (panelCount > 1) {
        panelsField.text = (panelCount - 1).toString
        renderActiveExpression
      }
    }
    disable = true
  }

  val panelIncButton = new Button {
    graphic = new Polygon {
      points ++= List(-5.0, 5.0, 0.0, 0.0, 5.0, 5.0)
      fill = Color.BLACK
      stroke = Color.BLACK
    }
    onAction = { () => 
      panelsField.text = (panelCount + 1).toString
      renderActiveExpression
    }
    disable = true
  }

  val panelsField = new TextField { 
    text = "1"
    editable = false 
    prefColumnCount = 2
    disable = true
  }

  val viewToggleVBox = new VBox {
    content = List(
      completeViewButton, 
      paneledViewButton,
      new HBox {
        padding = Insets(0,0,0,10)
        content = List(panelsLabel, panelIncButton, panelsField, panelDecButton)
        spacing = 5
      }
    )
    spacing = 5
  }

  val viewBorderBox = new BorderedTitledPane("View Style", viewToggleVBox)

  val panelLeftButton = new Button {
    graphic = new Polygon {
      points ++= List(0.0, 5.0, -5.0, 0.0, 0.0, -5.0)
      fill = Color.BLACK
      stroke = Color.BLACK
    }
    disable = true
    onAction = { () =>
      for {
        gallery <- activeGallery
      } {
        if (gallery.isInstanceOf[Spinner]) {
          gallery.asInstanceOf[Spinner].prev
        }
      }
    }
  }

  val panelRightButton = new Button {
    graphic = new Polygon {
      points ++= List(0.0, 5.0, 5.0, 0.0, 0.0, -5.0)
      fill = Color.BLACK
      stroke = Color.BLACK
    }
    disable = true
    onAction = { () =>
      for {
        gallery <- activeGallery
      } {
        if (gallery.isInstanceOf[Spinner]) {
          gallery.asInstanceOf[Spinner].next
        }
      }
    }
  }

  val zoomSlider = new Slider {
    min = .5
    max = 2
    value = 1
    showTickMarks = false
  }

  zoomSlider.value onChange { 
    for {
      gallery <- activeGallery
    } {
      if (gallery.isInstanceOf[StaticFrameworkGallery]) {
        val g = gallery.asInstanceOf[StaticFrameworkGallery]

        g.panels foreach { p => 
          p.setZoomFactor(zoomSlider.value().toDouble)
        }

        g.requestLayout
      } else {
        val g = gallery.asInstanceOf[FrameworkGallery]

        g.panels foreach { p => 
          p.setZoomFactor(zoomSlider.value().toDouble)
        }

        g.requestLayout
      }
    }
  }

  val zoomField = new TextField { 
    text = "100%"
    editable = false 
    prefColumnCount = 4
  }

  val zoomHBox =  new HBox {
    content = List (
      new Label("Zoom Factor"),
      zoomField,
      zoomSlider
    )
    spacing = 5
  }

  zoomHBox.setMaxSize(jfxsl.Region.USE_PREF_SIZE, jfxsl.Region.USE_PREF_SIZE)

  val controlBorderPane = new BorderPane {
    left = panelLeftButton
    right = panelRightButton
    center = zoomHBox
  }

  val controlBorderBox = new BorderedTitledPane("View Controls", controlBorderPane)

  val gridPane = new GridPane {
    styleClass += "orch-pane"
    hgap = 5
    vgap = 5
  }

  GridPane.setColumnSpan(viewerArea, 3)
  GridPane.setColumnSpan(controlBorderBox, 3)

  gridPane.add(viewerArea, 1, 1)
  gridPane.add(controlBorderBox, 1, 2)
  gridPane.add(viewBorderBox, 1, 3)

  borderPane.center = gridPane

  //============================================================================================
  // EVENT HANDLING
  //

  addEventFilter(KeyEvent.KEY_PRESSED,
    new EventHandler[KeyEvent] {
      def handle(ev : KeyEvent) {
        ev.getCode match {
          // case KeyCode.X => close
          case KeyCode.W => onWrite
          case _ => ()
        }
      }
    })


  def onShow = ()
  def onHide = ()

  def onWrite = {

    val fc = OrchardEditor.fileChooser
    fc.setTitle("Export Snapshot")

    val file = fc.showSaveDialog(getScene.getWindow)

    if (file != null) {

      val image = viewerArea.content.head.snapshot(new SnapshotParameters, null)

      try {
        ImageIO.write(SwingFXUtils.fromFXImage(image, null), "png", file)
      } catch {
        case e : java.io.IOException => {
          println("There was an error writing to the file!.")
        }
      }
    }
  }

}
