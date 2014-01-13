/**
  * SimpleJavaFXSVGGallery.scala - A JavaFX SVG Gallery implementation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.scene.web.WebEngine

import orchard.core._
import xml._

class SimpleJavaFXSVGGallery[A](labelEngine : WebEngine, seed : NCell[A]) extends JavaFXSVGGallery[A](labelEngine) {

  type PanelType = SimpleJavaFXSVGPanel[A]

  val complex : SimpleMutableComplex[A] = new SimpleMutableComplex(seed)

  def newPanel(i : Int) = new SimpleJavaFXSVGPanel(labelEngine, complex, i)

  def toSVG : Node = {
    // Okay.  We need to layout the panels correctly....
    val (w, h, panelData) = panelLayoutData
    // Hacky!
    val viewBoxStr = "-1.0 10.0 " ++ w.toString ++ " " ++ h.toString

    <svg width={w.toString} height={h.toString} viewBox={viewBoxStr} xmlns="http://www.w3.org/2000/svg" version="1.1">
      <script type="text/javascript">
        window.onload = function() {{ 
          alert("You loaded an opetopic gallery!"); 
        }}
      </script>
      <style>
        .orch-rect {{ 
          stroke: black; 
          stroke-width: 2; 
          fill: white 
        }} 
      
        .orch-rect:hover {{ 
          fill: red; 
        }} 

        .orch-label {{ 
          pointer-events: none; 
        }}
      </style>
      {panelData map (pInfo => {
        val (panel, x, y) = pInfo
        val translateStr = "translate(" ++ (x - panel.panelX).toString ++ "," ++ (y - panel.panelY).toString ++ ")"
        // Better would be to look up the attribute in the group ...
        <g transform={translateStr}>{panel.toSVG}</g>
      })}
    </svg>
  }

  initialize

}
