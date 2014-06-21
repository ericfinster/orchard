/**
  * FrameworkSVGPanel - An SVG Panel implementation for expression frameworks
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.javafx.svg

import scalafx.scene.web.WebEngine

import orchard.core.expression._

import xml._

class FrameworkSVGPanel(labelEngine : WebEngine, val complex : SimpleFramework, baseIndex : Int)
    extends JavaFXSVGPanel[Option[Expression]](labelEngine) { thisPanel =>

  type CellType = FrameworkSVGCell
  type EdgeType = FrameworkSVGEdge

  override type ComplexType = SimpleFramework

  override def refresh = ()

  class FrameworkSVGCell(val owner : complex.CellType) extends JavaFXSVGCell {

    def svgId : String = owner.hashCode.toString

    def labelSVG : NodeSeq =
      item match {
        case None => { <rect id={"cell-label-" ++ svgId} width="10" height="10" /> }
        case Some(expr) => { <text id={"cell-label-" ++ svgId}>{expr.id}</text> }
      }

    def classStr : String = 
      item match {
        case None => "orch-rect orch-cell-empty"
        case Some(expr) => "orch-rect orch-cell-" ++ expr.styleString
      }


    def hoverClassStr : String = 
      classStr ++ "-hover"

    def toSVG : NodeSeq = {
      val myRect : NodeSeq = <rect id={"cell-" ++ svgId} class={classStr} x={x.toString} y={y.toString} 
        rx="4" ry="4" width={width.toString} height={height.toString} />

      // I think if you make labelSVG and *element* you can use a scala method to change its attributes ...
      val labelX = x + width - labelWidth - internalPadding - strokeWidth
      val labelY = y + height - internalPadding - strokeWidth - (if (hasChildren) 0.0 else (1.5 * strokeWidth))

      // Should use the *owner's* hash code so that cells and edges have similar ids ....
      val myLabel : NodeSeq = 
        item match {
          case None => Seq.empty // <rect class="orch-label-filler" id={"cell-label-" ++ svgId} x={labelX.toString} y={labelY.toString} width="10" height="10" />
          case Some(expr) => <text class="orch-label" id={"cell-label-" ++ svgId} x={labelX.toString} y={labelY.toString}>{expr.id}</text>
        }

      canopy match {
        case None => myRect ++ myLabel
        case Some(tree) => myRect ++ myLabel ++ <g>{tree.toList map (c => c.toSVG)}</g>
      }
    }

    def cellInfoExpr : String = "document.gallery['cell-" ++ svgId ++ "']"
    def initStr : String = 
      "document.gallery['cell-" ++ svgId ++ "'] = " ++ objStr ++ 
        cellInfoExpr ++ ".cell.onclick = function () { document.gallery['cell-" ++ svgId ++ "'].doAlert(); };\n" ++
        cellInfoExpr ++ ".cell.onmouseover = " ++ mouseoverStr ++
        cellInfoExpr ++ ".cell.onmouseout = " ++ mouseoutStr ++
        faceArrayInitStr

    def objStr : String = {
      var str = "{\n" ++
        "cell : document.getElementById('cell-" ++ svgId ++ "'),\n" ++ 
        "edge : document.getElementById('edge-" ++ svgId ++ "'),\n" ++
        "extra : document.getElementById('extra-" ++ svgId ++ "'),\n" ++
        "doAlert : function () { alert('You clicked: " ++ svgId ++ "'); },\n" ++
        "doHover : " ++ hoverStr ++
        "doUnhover : " ++ unhoverStr ++
        "faces : []\n" ++
      "}\n"

      str
    }

    def hoverStr : String = 
      "function() {\n" ++
        "var cellInfo = document.gallery['cell-" ++ svgId ++ "'];\n" ++
        "cellInfo.cell.setAttribute('class', '" ++ hoverClassStr ++ "');\n" ++
        "if (cellInfo.extra != null) { cellInfo.extra.setAttribute('class', 'orch-extra-hover'); };\n" ++
      "},\n"

    def unhoverStr : String = 
      "function() {\n" ++
        "var cellInfo = document.gallery['cell-" ++ svgId ++ "'];\n" ++
        "cellInfo.cell.setAttribute('class', '" ++ classStr ++ "');\n" ++
        "if (cellInfo.extra != null) { cellInfo.extra.setAttribute('class', 'orch-extra'); };\n" ++
      "},\n"

    def mouseoverStr : String = 
      "function () {\n" ++
        "var cellInfo = document.gallery['cell-" ++ svgId ++ "'];\n" ++
        "cellInfo.doHover();\n" ++
        // "foreachFace(cellInfo, function(fc) { fc.doHover() });\n" ++
        "if (cellInfo.edge != null) { cellInfo.edge.setAttribute('class', 'orch-edge-hover'); };\n" ++
      "};\n"

    def mouseoutStr : String = 
      "function () {\n" ++
        "var cellInfo = document.gallery['cell-" ++ svgId ++ "'];\n" ++
        "cellInfo.doUnhover();\n" ++
        // "foreachFace(cellInfo, function(fc) { fc.doUnhover() });\n" ++
        "if (cellInfo.edge != null) { cellInfo.edge.setAttribute('class', 'orch-edge'); };\n" ++
      "};\n"

    def faceArrayInitStr : String = {
      var faceStr : String = ""
      var idx : Int = 0

      owner.skeleton map (face => {
        faceStr ++= "document.gallery['cell-" ++ svgId ++ "'].faces[" ++ idx.toString ++ "] = document.gallery['cell-" ++ face.hashCode.toString ++ "'];\n"
        idx += 1
      })

      faceStr
    }
  }

  class FrameworkSVGEdge(val owner : complex.CellType) extends JavaFXSVGEdge {

    def svgId : String = owner.hashCode.toString

    def toSVG : NodeSeq =
      <path id={"edge-" ++ svgId} d={pathString} class="orch-edge"/>

  }

  def newCell(owner : complex.CellType) = { 
    val frameworkCell = new FrameworkSVGCell(owner)
    owner.registerPanelCell(thisPanel)(frameworkCell)
    frameworkCell
  }

  def newEdge(owner : complex.CellType) = {
    val frameworkEdge = new FrameworkSVGEdge(owner)
    owner.registerPanelEdge(thisPanel)(frameworkEdge)
    frameworkEdge
  }

  //============================================================================================
  // INITIALIZATION
  //

  var baseCell : FrameworkSVGCell = newCell(complex.baseCells(baseIndex))

  refreshPanelData
}

