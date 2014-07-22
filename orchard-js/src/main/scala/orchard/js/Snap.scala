/**
  * Snap.scala - Simple Interface to Snap
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.js

import scala.scalajs._

import org.scalajs.dom

trait Paper extends js.Object {

  def circle(x : js.Number, y : js.Number, r : js.Number) : Element
  def text(x : js.Number, y : js.Number, text : js.String) : Element
  def rect(x : js.Number, y : js.Number, width : js.Number, height : js.Number) : Element
  def rect(x : js.Number, y : js.Number, width : js.Number, height : js.Number, rx : js.Number, ry : js.Number) : Element
  def path(pathString : js.String) : Element
  def g(args : js.Array[Element]) : Element

  def clear : Unit

  def attr(params : js.Object) : Unit

}

trait Element extends js.Object {

  def getBBox() : BBox

  def attr(params : js.Object) : Unit
  def append(el : Element) : Unit

  def mouseover(handler : js.Function) : Unit
  def mouseout(handler : js.Function) : Unit

}

trait BBox extends js.Object {

  def x : js.Number
  def x2 : js.Number

  def y : js.Number
  def y2 : js.Number

  def h : js.Number
  def height : js.Number

  def w : js.Number
  def width : js.Number

}

object Snap extends js.Object {

  def apply(width : js.Number, height : js.Number) : Paper = ???
  def apply(query : String) : Paper = ???
  def apply(d : dom.Element) : Paper = ???

}
