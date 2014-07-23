package controllers

import play.api._
import play.api.mvc._

import libs.json._

import orchard.core.cell._
import orchard.core.util._
import orchard.core.complex._
import orchard.core.checker._

import models.PlayComplex

object Application extends Controller {

  object WorksheetWriter {

    import models.PlayJson._

    implicit def worksheetWrites : Writes[Worksheet] = 
      new Writes[Worksheet] {
        def writes(worksheet : Worksheet) : JsValue = {
          val markerWriter = implicitly[JsonWritable[WorksheetMarker, JsValue]]
          worksheet.toJson(PlayJsonWriter, markerWriter)
        }
      }

  }

  val worksheet = Worksheet()

  def getWorksheet = Action {
    import WorksheetWriter._
    val json = Json.toJson(worksheet)
    Ok(json)
  }

  //============================================================================================
  // OLD STUFF
  //

  def index = Action {
    Ok(views.html.index("Orchard Index"))
  }

  def getComplex = Action { 
    val complex = new PlayComplex(Example.Psi)
    val json = Json.toJson(complex)
    Ok(json)
  }

}
