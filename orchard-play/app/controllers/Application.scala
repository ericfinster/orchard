package controllers

import play.api._
import play.api.mvc._

import libs.json._

import orchard.core.cell._
import orchard.core.util._
import orchard.core.complex._
import orchard.core.checker._

import ErrorM._
import models.PlayComplex

object Application extends Controller {

  val worksheet = Worksheet()

  def worksheetJson = {
    import models.OrchardToPlay._
    Json.toJson(worksheet.toMarkerComplex)
  }

  def getWorksheet = Action {
    Logger.debug("Getting the worksheet data.")
    Ok(worksheetJson)
  }

  def debug = Action(BodyParsers.parse.json) { request =>
    Logger.debug("Received a json post:")
    Logger.debug(Json.prettyPrint(request.body))
    Ok(Json.obj("status" -> "OK", "message" -> "all done"))
  }

  def newModule = Action(BodyParsers.parse.json) { request =>

    // Grab the name from the json body
    val moduleId = (request.body \ "moduleId").as[String]

    Ok(views.html.module(moduleId))

  }

  def extrude = Action(BodyParsers.parse.json) { request =>
    import models.OrchardToPlay._

    Logger.debug("Got an extrude request ...")
    // Logger.debug(Json.prettyPrint(request.body))

    val descriptorResult = request.body.validate[SelectionDescriptor]

    descriptorResult.fold(
      errors => { 
        Logger.debug("Could not parse the request.")
        BadRequest(Json.obj("status" -> "KO", "message" -> JsError.toFlatJson(errors)))
      },
      descriptor => {
        val extrusionResult = 
          for {
            _ <- worksheet.selectFromDescriptor(descriptor)
            _ <- worksheet.emptyExtrusion
          } yield ()

        extrusionResult match {
          case Right(()) => {
            Ok(Json.obj("status" -> "OK", "message" -> worksheetJson))
          }
          case Left(msg) => {
            Logger.debug("There was an error in semantics.")
            BadRequest(Json.obj("status" -> "KO", "message" -> msg))
          }
        }
      }
    )
  }

  //============================================================================================
  // OLD STUFF
  //

  def index = Action {
    Ok(views.html.index("This is some content which goes in a panel."))
  }

  def getComplex = Action { 
    val complex = new PlayComplex(Example.Psi)
    val json = Json.toJson(complex)
    Ok(json)
  }

}
