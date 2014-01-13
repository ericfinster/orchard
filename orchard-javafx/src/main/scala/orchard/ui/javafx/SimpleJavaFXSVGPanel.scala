/**
  * SimpleJavaFXSVGPanel.scala - A simple text based SVG Panel implementation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx

import scalafx.scene.web.WebEngine

import orchard.core._
import xml._

class SimpleJavaFXSVGPanel[A](engine : WebEngine, val complex : SimpleMutableComplex[A], baseIndex : Int) extends JavaFXSVGPanel[A](engine) { thisPanel =>

  type CellType = SimpleJavaFXSVGCell
  type EdgeType = SimpleJavaFXSVGEdge

  type ComplexType = SimpleMutableComplex[A]

  def refresh = ()

  def panelSVG : NodeSeq = {
    val viewBoxStr = (baseCell.x - 10).toString ++ " " ++ (baseCell.y - 10).toString ++ " " ++ 
      (baseCell.width + 20).toString ++ " " ++ (baseCell.height + 20).toString

    // Rokay.  Here's a fairly interesting version which seems to output just fine.
    <svg xmlns="http://www.w3.org/2000/svg" version="1.1" width="400" height="400" viewBox={viewBoxStr}>
      <script type="text/javascript">
        window.onload = function() {{ 
          alert("You loaded an opetopic panel!"); 
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
      {toSVG}
    </svg>
  }

  class SimpleJavaFXSVGCell(val owner : complex.SimpleMutableCell) extends JavaFXSVGCell {

    def svgId : String = "cell-" ++ owner.hashCode.toString

    def labelSVG : NodeSeq = <text id={svgId}>{item.toString}</text>

    def toSVG : NodeSeq = {
      val myRect : NodeSeq = <rect class="orch-rect" x={x.toString} y={y.toString} rx="4" ry="4" width={width.toString} height={height.toString} />

      // I think if you make labelSVG and *element* you can use a scala method to change its attributes ...
      val labelX = x + width - labelWidth - internalPadding - strokeWidth
      val labelY = y + height - internalPadding - strokeWidth - (if (hasChildren) 0.0 else (1.5 * strokeWidth))

      // Should use the *owner's* hash code so that cells and edges have similar ids ....
      val myLabel : NodeSeq = <text class="orch-label" x={labelX.toString} y={labelY.toString} id={svgId}>{item.toString}</text>

      canopy match {
        case None => myRect ++ myLabel
        case Some(tree) => myRect ++ myLabel ++ <g>{tree.toList map (c => c.toSVG)}</g>
      }
    }

  }

  class SimpleJavaFXSVGEdge(val owner : complex.SimpleMutableCell) extends JavaFXSVGEdge {

    def toSVG : NodeSeq = 
      <path d={pathString} stroke="black" stroke-width="2" fill="none" />

  }

  def newCell(owner : complex.SimpleMutableCell) : SimpleJavaFXSVGCell = {
    val simpleCell = new SimpleJavaFXSVGCell(owner)
    owner.registerPanelCell(thisPanel)(simpleCell)
    simpleCell
  }

  def newEdge(owner : complex.SimpleMutableCell) : SimpleJavaFXSVGEdge = {
    val simpleEdge = new SimpleJavaFXSVGEdge(owner)
    owner.registerPanelEdge(thisPanel)(simpleEdge)
    simpleEdge
  }

  //============================================================================================
  // UI INITIALIZATION
  //

  var baseCell : SimpleJavaFXSVGCell = newCell(complex.baseCells(baseIndex))

  refreshPanelData

}
