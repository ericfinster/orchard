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

  val workspace = new Workspace

  def newWorksheet = Action {
    import models.OrchardToPlay._
    val worksheet = workspace.newWorksheet
    Logger.debug("Returning a worksheet with id: " ++ worksheet.hashCode.toString)
    Ok(Json.toJson(success(worksheet.toMarkerComplex)))
  }

  def requestWorksheet = Action(BodyParsers.parse.json) { request =>
    import models.OrchardToPlay._
    val worksheetId = (request.body \ "worksheetId").as[Int]

    val markerComplex = 
      for {
        worksheet <- workspace.getWorksheet(worksheetId)
      } yield worksheet.toMarkerComplex

    Ok(Json.toJson(markerComplex))
  }

  def extrudeWorksheet = Action(BodyParsers.parse.json) { request =>
    import models.OrchardToPlay._

    val worksheetId = (request.body \ "worksheetId").as[Int]
    val selectionDescriptor = (request.body \ "selectionDescriptor").as[SelectionDescriptor]

    val extrudedWorksheet = 
      for {
        worksheet <- workspace.getWorksheet(worksheetId)
        _ <- worksheet.selectFromDescriptor(selectionDescriptor)
        _ <- ensure(worksheet.selectionIsExtrudable, "Selection is not extrudable.")
        _ <- worksheet.emptyExtrusion
      } yield worksheet.toMarkerComplex


    Ok(Json.toJson(extrudedWorksheet))

  }

  def newModule = Action(BodyParsers.parse.json) { request =>
    import models.OrchardToPlay._

    val moduleId = (request.body \ "moduleId").as[String]
    val checkerAddress = (request.body \ "address").as[CheckerAddress]

    Logger.debug("Requesting module: " ++ moduleId)

    val insertionCommand = workspace.insertModule(moduleId)

    val result = 
      for {
        moduleNode <- workspace.runCommandAtAddress(insertionCommand, checkerAddress)
      } yield moduleNode.name


    Ok(Json.toJson(result))
  }

  def newParameter = Action(BodyParsers.parse.json) { request => 

    import models.OrchardToPlay._

    val worksheetId = (request.body \ "worksheetId").as[Int]
    val cellAddress = (request.body \ "cellAddress").as[CellAddress]
    val identString = (request.body \ "identString").as[String]
    val isThin = (request.body \ "isThin").as[Boolean]
    val checkerAddress = (request.body \ "checkerAddress").as[CheckerAddress]

    val parameterResult = 
      workspace.newParameter(
        worksheetId,
        cellAddress,
        identString,
        isThin,
        checkerAddress
      )

    Ok(Json.toJson(parameterResult))

  }

  def newDefinition = Action(BodyParsers.parse.json) { request => 

    import models.OrchardToPlay._

    val worksheetId = (request.body \ "worksheetId").as[Int]
    val cellAddress = (request.body \ "cellAddress").as[CellAddress]
    val identString = (request.body \ "identString").as[String]
    val checkerAddress = (request.body \ "checkerAddress").as[CheckerAddress]

    val definitionResult = 
      workspace.newDefinition(
        worksheetId,
        cellAddress,
        identString,
        checkerAddress
      )

    Ok(Json.toJson(definitionResult))

  }

  def requestEnvironment = Action(BodyParsers.parse.json) { request =>
    import models.OrchardToPlay._

    val checkerAddress = (request.body \ "address").as[CheckerAddress]

    val envResult = workspace.runCommandAtAddress(
      workspace.environmentTree, 
      checkerAddress
    ) 

    Ok(Json.toJson(envResult))

  }

  //============================================================================================
  // OLD STUFF
  //

  def index = Action {
    workspace.reset
    Ok(views.html.index("This is some content which goes in a panel."))
  }

  def template = Action {
    Ok(views.html.template("Template"))
  }

  def debug = Action(BodyParsers.parse.json) { request =>
    Logger.debug("Received a json post:")
    Logger.debug(Json.prettyPrint(request.body))
    Ok(Json.obj("status" -> "OK", "message" -> "all done"))
  }

}
