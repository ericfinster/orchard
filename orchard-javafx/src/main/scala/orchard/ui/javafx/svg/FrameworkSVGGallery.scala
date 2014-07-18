/**
  * FrameworkSVGGallery.scala - An SVG Gallery for displaying expression frameworks
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx.svg

import scalafx.scene.web.WebEngine

import orchard.core.cell._
import orchard.core.expression._

import xml._

class FrameworkSVGGallery(labelEngine : WebEngine, seed : NCell[Option[Expression]]) extends JavaFXSVGGallery[Option[Expression]](labelEngine) {

  type PanelType = FrameworkSVGPanel

  val complex : SimpleFramework = new SimpleFramework(seed)

  def newPanel(i : Int) = new FrameworkSVGPanel(labelEngine, this, i)

  // We would like to instead keep a gallery array in which we can have multiple galleries.  This way,
  // we can have more than one active gallery on a page.  Right now, more recent galleries overwrite the
  // previous ones ....

  // Also, you should split the css into a separate main stylesheet which is common to all opetopic renderings ...
  // (This way, you can set global font information and share it so that they can be easily embedded in other
  // webpages)

  def initStr : String = {
    var str : String = "document." ++ svgId ++ " = { };"
    forallCells (cell => str ++= cell.initStr)
    str ++= faceIterator
    str
  }

  def faceIterator : String =
    "function foreachFace(c, f) {" ++
      "for (var i = 0; i < c.faces.length; i++) {" ++
      "  f(c.faces[i])" ++
      "}" ++
      "}"

  def svgId : String = "gallery" ++ this.hashCode.toString

  def toSVG : Node = {
    // Okay.  We need to layout the panels correctly....
    val (w, h, panelData) = panelLayoutData
    // Hacky!
    val viewBoxStr = "-1.0 10.0 " ++ w.toString ++ " " ++ h.toString

    <svg width={w.toString} height={h.toString} viewBox={viewBoxStr} xmlns="http://www.w3.org/2000/svg" version="1.1">
      <script type="text/javascript">
        window.addEventListener('load', function() {{ {initStr} }}, false)
      </script>
      <style>
        path {{
          stroke: black;
          stroke-width: 2;
        }}
        .orch-rect {{ 
          stroke: black; 
          stroke-width: 2; 
          fill: white 
        }} 
        .orch-edge {{
          stroke: black;
          stroke-width: 2;
          fill: none;
        }}
        text {{
          font-family: "Liberation Sans", Helvetica, sans-serif;
          font-size: 13px;
          font-style: normal;
        }}
        .orch-edge-hover {{
          stroke: red;
          stroke-width: 2;
          fill: none;
        }}
        .orch-label {{ 
          pointer-events: none; 
        }}
        .orch-label-filler {{
          pointer-events: none;
          stroke: white;
          fill white;
        }}
        .orch-extra text {{ fill: black; }}
        .orch-extra-hover text {{ fill: red; }}
        .orch-extra path {{ stroke: black; fill: black; }}
        .orch-extra-hover path {{ stroke: red; fill: red; }}
        .orch-cell-empty {{ fill: white }}
        .orch-cell-exposed {{ fill: #F0FBDC }}
        .orch-cell-variable {{ fill: #FFF5E0 }}
        .orch-cell-variable-thin {{ fill: #E2D1EC }}
        .orch-cell-filler {{ fill: #D3D9EC }}
        .orch-cell-bdry {{ fill: #FFE0E0 }}
        .orch-cell-bdry-thin {{ fill: #D3D9EC }}
        .orch-cell-app {{ fill: #F5D7E6 }}
        .orch-cell-empty-hovered {{ fill: #838EAF }}
        .orch-cell-exposed-hovered {{ fill: #C1D89B }}
        .orch-cell-variable-hovered {{ fill:  #E4CFA3 }}
        .orch-cell-variable-thin-hovered {{ fill: #9D7FAE }}
        .orch-cell-filler-hovered {{ fill: #838EAF }}
        .orch-cell-bdry-hovered {{ fill: #E4A3A3 }}
        .orch-cell-bdry-thin-hovered {{ fill: #838EAF }}
        .orch-cell-app-hovered {{ fill: #C78EAB }}
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
