/**
  * Editor.scala - Orchard-Lift Editor
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package orchard.ui.lift.snippet

import net.liftweb.http.SHtml
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.JE._

import net.liftweb.json.JsonAST._

import xml._
import orchard.ui.lift._
import orchard.core.util._
import orchard.core.cell._
import orchard.core.complex._

object Editor {

  // Great, so here we create a complex.  The idea will be to serialize
  // this guy and send him off to the client ...

  val complex = new LiftComplex[String](Example1.w map (_.toString))
  val stringWriter = implicitly[JsonWritable[String, JValue]]
  val complexJson = complex.toJson(LiftJsonWriter, stringWriter)

  def example(html : NodeSeq) : NodeSeq = 
    SHtml.a(() => Call("orchard.js.Main().renderComplex", complexJson), Text("Render complex."))

  // def createSnap(html : NodeSeq) : NodeSeq = 
  //   SHtml.a(() => Call("orchard.js.Main().createSnapSurface", complexJson), Text("Create Snap Surface."))

  // def createSvg(html : NodeSeq) : NodeSeq = 
  //   SHtml.a(() => Call("orchard.js.Main().createSVGElement", complexJson), Text("Create Svg Surface."))


}
