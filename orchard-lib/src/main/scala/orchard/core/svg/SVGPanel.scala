/**
  * SVGPanel.scala - A Panel which can render its contents to SVG
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.core.svg

import scala.collection.mutable.ListBuffer

import orchard.core.ui._
import orchard.core.complex._

import xml._

trait SVGPanel[A] extends SizeablePanel[A] {

  override type CellType <: SVGCell
  override type EdgeType <: SVGEdge

  def panelId : String = hashCode.toString

  def panelInitString = 
    "orchard.registerPanel(" ++ panelId ++ ", " ++ panelObjectString ++ ");"

  def panelObjectString = {

    val cellDeclarations = ListBuffer.empty[String]
    val edgeDeclarations = ListBuffer.empty[String]

    baseCell.foreachCell (cell => {
      cellDeclarations += (cell.cellId ++ " : " ++ cell.cellObjectString)
    })

    for { tgt <- baseCell.target } {
      tgt foreachEdge (edge => {
        edgeDeclarations += (edge.edgeId ++ " : " ++ edge.edgeObjectString)
      })
    }

    val leftEdgeId : Option[String] = 
      for {
        srcs <- baseCell.sources
        hd <- srcs.headOption
      } yield hd.edgeId

    val rightEdgeId : Option[String] = 
      for {
        srcs <- baseCell.sources
        lst <- srcs.lastOption
      } yield lst.edgeId

    val objString = 
      "{ " ++
        "baseCell : " ++ baseCell.cellId ++ "," ++
        "leftEdge : " ++ (leftEdgeId getOrElse "null") ++ "," ++
        "rightEdge : " ++ (rightEdgeId getOrElse "null") ++ "," ++
        "cells : " ++ cellDeclarations.mkString("{ ", ", ", " },") ++ 
        "edges : " ++ edgeDeclarations.mkString("{ ", ", ", " }") ++
      "}"

    objString
  }

  def toSVG : NodeSeq = {
    <svg id={"panel-svg-" ++ panelId} xmlns="http://www.w3.org/2000/svg" version="1.1">
    <script type="text/javascript">
      $(function() {{
        { panelInitString ++ "; orchard.renderPanel(" ++ panelId ++ ");" }
      }})
    </script>
    <style>
      rect {{
        stroke: black;
        stroke-width: 2;
        fill: white
      }}
      path {{
        stroke: black;
        stroke-width: 2;
        fill: none;
      }}
    </style>
      { panelGroup }
    </svg>

  }

  def panelGroup : NodeSeq = {
    var pathNodes : NodeSeq = Seq.empty

    for { tgt <- baseCell.target } {
      tgt foreachEdge (edge => pathNodes ++= edge.toSVG)
    }

    <g id={"panel-" ++ panelId}>{ baseCell.toSVG ++ pathNodes }</g>
  }

  def bufferedAttrString(name : String, history : ListBuffer[SizeResult]) : String = {
    val historyArray =
      for {
        sizeEntry <- history
      } yield {
        "function() { return (" ++ sizeEntry.jsString ++ ") }"
      }

    name ++ " : function(i) { var history = " ++ 
      historyArray.mkString("[ ", ", ", " ]") ++ "; return history[i]() }"
  }

  def attrString(name : String, size : SizeResult) : String = {
    name ++ " : function() {return (" ++ size.jsString ++ ") }"
  }

  trait SVGCell extends SizeableCell { thisCell : CellType =>

    def cellId : String = hashCode.toString
    def labelId : String = "label-" ++ cellId
    def rectId : String = "rect-" ++ cellId

    def cellObjectString = {
      val attrBuf : ListBuffer[(String, SizeResult)] = ListBuffer(
        ("x", x),
        ("y", y),
        ("width", width),
        ("height", height),
        ("labelX", labelX),
        ("labelY", labelY)
      )

      val bufAttrBuf : ListBuffer[(String, ListBuffer[SizeResult])] = ListBuffer(
        ("rootX", rootXBuffer),
        ("rootY", rootYBuffer),
        ("rootLeftMargin", rootLeftMarginBuffer),
        ("rootRightMargin", rootRightMarginBuffer),
        ("internalWidth", internalWidthBuffer),
        ("internalHeight", internalHeightBuffer),
        ("labelPadding", labelPaddingBuffer)
      )

      val labelWidthAttr = "labelWidth : function() { return $(\"#" ++ labelId ++ "\").width() }"
      val labelHeightAttr = "labelHeight : function() { return $(\"#" ++ labelId ++ "\").height() }"

      val attrs = 
        (attrBuf map { case (name, size) => attrString(name, size) }) ++
          (bufAttrBuf map { case (name, buf) => bufferedAttrString(name, buf) }) ++
          ListBuffer(labelWidthAttr, labelHeightAttr)

      attrs.mkString("{ ", ", ", " }")
    }

    def toSVG : NodeSeq = {
      val myRect : NodeSeq = <rect id={rectId} rx="4" ry="4" x="20" y="20" width="10" height="10"/>
      val myLabel : NodeSeq = <text id={labelId} x="50" y="50">{ item.toString }</text> 

      canopy match {
        case None => myRect ++ myLabel
        case Some(tree) => myRect ++ myLabel ++ <g>{tree.toList map (c => c.toSVG)}</g>
      }
    }

  }

  trait SVGEdge extends SizeableEdge { thisEdge : EdgeType =>

    def edgeId = hashCode.toString

    def edgeObjectString = {
      val attrBuf : ListBuffer[(String, SizeResult)] = ListBuffer(
        ("incomingX", incomingX),
        ("incomingY", incomingY),
        ("outgoingX", outgoingX),
        ("outgoingY", outgoingY)
      )

      val attrs = attrBuf map { case (name, size) => attrString(name, size) }

      attrs.mkString("{ ", ", ", " }")
    }

    def toSVG : NodeSeq = {
      <path id={"edge-" ++ hashCode.toString} />
    }

  }

  implicit class JsSize[A](size : SizeExpression[A]) {

    def jsString : String =
      size match {
        case Constant(value) => value.toString
        case Plus(e, f) => "( " ++ e.jsString ++ " + " ++ f.jsString ++ " )"
        case Minus(e, f) => "( " ++ e.jsString ++ " - " ++ f.jsString ++ " )"
        case Max(e, f) => "Math.max( " ++ e.jsString ++ ", " ++ f.jsString ++ " )"
        case Divide(e, f) => "( " ++ e.jsString ++ " / " ++ f.jsString ++ " )"
        case Times(e, f) => "( " ++ e.jsString ++ " * " ++ f.jsString ++ " )"
        case If(cond, e, f) => 
          "((function () { " ++
            "if (" ++ cond.jsString ++ ") " ++
            "{ return (" ++ e.jsString ++ ") } " ++
            "else { return(" ++ f.jsString ++ ") } })())"
        case orchard.core.ui.Attribute(ref, attr) => "orchard.getCell(" ++ panelId ++ ", " ++ ref.toString ++ ")." ++ attr
      }
    
  }

  implicit class JsCond[A](cond : SizeCondition[A]) {
    
    def jsString : String =
      cond match {
        case Gt(e, f) => e.jsString ++ " > " ++ f.jsString
        case Gte(e, f) => e.jsString ++ " >= " ++ f.jsString
        case Lt(e, f) => e.jsString ++ " < " ++ f.jsString
        case Lte(e, f) => e.jsString ++ " <= " ++ f.jsString
        case Eq(e, f) => e.jsString ++ " == " ++ f.jsString
      }

  }

}

