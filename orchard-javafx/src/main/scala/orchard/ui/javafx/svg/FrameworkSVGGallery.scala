/**
  * FrameworkSVGGallery.scala - An SVG Gallery for displaying expression frameworks
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx.svg

import scalafx.scene.web.WebEngine

import orchard.core.ui._
import orchard.core.cell._
import orchard.core.complex._
import orchard.core.expression._

import xml._

class FrameworkSVGGallery(labelEngine : WebEngine, seed : NCell[Option[Expression]]) extends JavaFXSVGGallery[Option[Expression]](labelEngine) {

  type PanelType = FrameworkSVGPanel

  val complex : SimpleFramework = new SimpleFramework(seed)

  def newPanel(i : Int) = new FrameworkSVGPanel(labelEngine, complex, i)

  // For each cell, we want to add a member to the gallery object ...
  def initStr : String = {
    var str : String = "document.gallery = { };\n"
    forallCells (cell => str ++= cell.initStr)
    str ++= faceIterator
    str ++= "alert('Initialization Finished.');\n"
    str
  }

  def faceIterator : String =
    "function foreachFace(c, f) {\n" ++
      "for (var i = 0; i < c.faces.length; i++) {\n" ++
      "  f(c.faces[i])\n" ++
      "}\n" ++
      "}\n"

  def toSVG : Node = {
    // Okay.  We need to layout the panels correctly....
    val (w, h, panelData) = panelLayoutData
    // Hacky!
    val viewBoxStr = "-1.0 10.0 " ++ w.toString ++ " " ++ h.toString

    <svg width={w.toString} height={h.toString} viewBox={viewBoxStr} xmlns="http://www.w3.org/2000/svg" version="1.1">
      <script type="text/javascript">
        window.onload = function() {{ 
          {initStr}
        }}
      </script>
      <style>
        .root {{
          blue-2: #838EAF;
          blue-5: #D3D9EC;

          peach-2: #E4CFA3;
          peach-5: #FFF5E0;

          violet-2: #9D7FAE;
          violet-5: #E2D1EC;

          red-2: #E4A3A3;
          red-5: #FFE0E0;

          empty-base: white;
          empty-hover: blue-2;

          var-base: peach-5;
          var-hover: peach-2;

          var-thin-base: violet-5;
          var-thin-hover: violet-2;

          filler-base: blue-5;
          filler-hover: blue-2;
              
          filler-tgt-base: red-5;
          filler-tgt-hover: red-2;

          filler-tgt-thin-base: blue-5;
          filler-tgt-thin-hover: blue-2;
        }}

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

        .orch-edge-hover {{
          stroke: red;
          stroke-width: 2;
          fill: none;
        }}

        .orch-extra text {{ fill: black; }}
        .orch-extra-hover text {{ fill: red; }}
        .orch-extra path {{ stroke: black; fill: black; }}
        .orch-extra-hover path {{ stroke: red; fill: red; }}

        .expr-cell-empty {{ fill: white; }}
        .expr-cell-var {{ fill: #FFF5E0; }}
        .expr-cell-var-thin {{ fill: #9D7FAE; }}
        .expr-cell-filler {{ fill: #D3D9EC; }}
        .expr-cell-filler-tgt {{ fill: #FFE0E0; }}
        .expr-cell-filler-tgt-thin {{ fill: #D3D9EC; }}

        .expr-cell-empty-hover {{ fill: red; }}
        .expr-cell-var-hover {{ fill: #E4CFA3; }}
        .expr-cell-var-thin-hover {{ fill: #9D7FAE; }}
        .expr-cell-filler-hover {{ fill: #838EAF; }}
        .expr-cell-filler-tgt-hover {{ fill: #E4A3A3; }}
        .expr-cell-filler-tgt-thin-hover {{ fill: #838EAF; }}

        .orch-label {{ 
          pointer-events: none; 
        }}

        .orch-label-filler {{
          pointer-events: none;
          stroke: white;
          fill white;
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
