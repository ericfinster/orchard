/**
  * Editor.scala - Orchard-Lift Editor
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.lift.snippet


import net.liftweb.common._

import net.liftweb.http.LiftRules

import net.liftweb.util._
import Helpers._

import net.liftweb.http.SHtml
import net.liftweb.http.js._
import JE.JsRaw
import JE.Call
import net.liftweb.http.js.JsCmds._

import net.liftweb.common.Loggable
import net.liftweb.json.JsonAST._
import net.liftweb.json.DefaultFormats

import xml._
import orchard.ui.lift._
import orchard.core.cell._
import orchard.core.complex._

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap

object Editor {

  // Sweet.  So this works nicely.  You set the right class and just blaze through the text elements,
  // returning all the bounding boxes over to this side.  Now we just parse the information, pass it
  // back to the panel, and try to cause a reloading of the data ...

  // We can cause a reload of the data by using the SetHtml javascript command.
  // Right.  And the cookbook shows how to make this come from a complete template
  // if you like.  But either way, it seems to use SetHtml, so do we care?

  val panel = new SimplePanel(new SimpleMutableComplex(Example.Psi), 2)

  def processDimensions(value : JValue) : JsCmd = {
    val dimensionMap : Map[String, BBox] = new HashMap[String, BBox]

    implicit val formats = DefaultFormats

    // We should catch the possible exception here
    value match {
      case JObject(members) => {
        for {
          JField(id, bbox) <- members
        } { dimensionMap(id) = bbox.extractOpt[BBox].get }
      }
      case _ => println("Ooops, it's not.")
    }

    panel.setLabelsFromBBox(dimensionMap)
    panel.render

    SetHtml("viewer", panel.toSVG)
  }

  def render = <svg width="400" height="400">{panel.labelProofSheet}</svg>

  def buttonClicked = "button [onclick]" #> SHtml.jsonCall(Call("returnLabelDimensions"), processDimensions _)
}
